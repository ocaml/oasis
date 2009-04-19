
(** Various tool to debug OASIS module
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISAstTypes;;
open Genlex;;

let stream_debugger st = 
  Stream.from
    (fun _ ->
       try
         let c =
           Stream.next st
         in
           prerr_char c; flush stderr;
           Some c
       with Stream.Failure ->
         None)
;;

let parse_file ~debug fn = 
  let chn =
    open_in fn
  in

  let st =
    Stream.of_channel chn
  in

  (* Get rid of MS-DOS file format *)
  let dos2unix st = 
    Stream.from
      (fun _ ->
         try
           match Stream.next st with 
             | '\r' when Stream.peek st = Some '\n' ->
                 Some ' '
             | c ->
                 Some c
         with Stream.Failure ->
           None)
  in

  (* Get rid of '\t' for indent *)
  let notab st =
    Stream.from
      (fun _ ->
         try
           match Stream.next st with 
             | '\t' ->
                 Some ' '
             | c ->
                 Some c
         with Stream.Failure ->
           None)
  in

  (* Skip comment *)
  let skip_comment st =
    Stream.from
      (fun _ ->
         try 
           match Stream.next st with
             | '#' ->
                 while Stream.next st <> '\n' do
                   ()
                 done;
                 Some '\n'
             | c -> 
                 Some c
         with Stream.Failure ->
           None)
  in

  (* Regroup freeform data inside a single string *)
  let string_of_freeform st =
    let indent_level =
      ref 0
    in
    let indent_count =
      ref true
    in
    let buffer = 
      Queue.create ()
    in
    let rec skip_blank () = 
      match Stream.peek st with 
        | Some ' ' | Some '\t' | Some '\r' ->
            Stream.junk st;
            skip_blank ()
        | _ ->
            ()
    in
    let rec input_line () = 
      match Stream.peek st with 
        | Some '\n' | None ->
            ()
        | Some c ->
            Queue.push c buffer;
            Stream.junk st;
            input_line ()
    in
    let rec check_continuation () = 
      let continuation_indent = 
        !indent_level + 1
      in
      let begin_next_line =
        Stream.npeek (continuation_indent + 1) st
      in
      let begin_expected =
        '\n' :: (Array.to_list (Array.make continuation_indent ' '))
      in
        if begin_next_line = begin_expected then
          (
            for i = 0 to continuation_indent do 
              Stream.junk st
            done;
            match Stream.npeek 2 st with 
              | ['.'; '\n'] -> 
                  Queue.push '\\' buffer;
                  Queue.push 'n' buffer;
                  Stream.junk st;
                  check_continuation ()
              | _ ->
                  Queue.push ' ' buffer;
                  input_line ();
                  check_continuation ()
          )
        else
          (
            ()
          )
    in
    let lookup_freeform () = 
      Queue.push '"' buffer;
      skip_blank ();
      input_line ();
      check_continuation ();
      Queue.push '"' buffer
    in
      Stream.from
        (fun _ ->
           try
             if Queue.is_empty buffer then
               (
                 match Stream.next st with
                   | ':' ->
                       lookup_freeform ();
                       Some ':'
                   | '\n' as c ->
                       indent_count := true;
                       indent_level := 0;
                       Some c
                   | ' ' | '\t' as c ->
                       if !indent_count then
                         incr indent_level;
                       Some c
                   | c ->
                       indent_count := false;
                       Some c
               )
             else
               (
                 Some (Queue.pop buffer)
               )
           with Stream.Failure ->
             None)
  in

  (* Add required { } regarding indentation *)
  let braces_of_indent st =
    let former_line_indent =
      ref 0
    in
    let cur_line_indent =
      ref 0
    in
    let indent_count =
      ref true
    in
    let last_line =
      ref false
    in
    let brace_buffer =
      Queue.create ()
    in
    let compensate_diff_indent () = 
      let diff_indent =
        !former_line_indent - !cur_line_indent
      in
      let brace = 
        if diff_indent < 0 then
          '{'
        else
          '}'
      in
        for i = 1 to abs diff_indent do 
          Queue.push brace brace_buffer
        done
    in
      Stream.from
        (fun _ ->
           try
             if Queue.is_empty brace_buffer then
               (
                 match Stream.next st with 
                   | '\n' as c ->
                       (* Don't take into account blank line *)
                       if not !indent_count then
                         former_line_indent := !cur_line_indent;
                       indent_count := true;
                       cur_line_indent := 0;
                       Some c
                   | ' ' | '\t' as c ->
                       if !indent_count then
                         incr cur_line_indent;
                       Some c
                   | c ->
                       if !indent_count then
                         (
                           indent_count := false;
                           compensate_diff_indent ();
                           Queue.push c brace_buffer;
                           Some (Queue.pop brace_buffer)
                         )
                       else
                         (
                           Some c
                         )
               )
             else
               (
                 Some (Queue.pop brace_buffer)
               )
           with Stream.Failure ->
             (
               if not !last_line then
                 (
                   if not !indent_count then
                     former_line_indent := !cur_line_indent;
                   cur_line_indent := 0;
                   last_line := true;
                   compensate_diff_indent ()
                 );

               if Queue.is_empty brace_buffer then
                 None
               else
                 Some (Queue.pop brace_buffer)
             )
        )
  in

  (* Count line *)
  let lineno =
    ref 1
  in
  let charno =
    ref 0
  in
  let count_line st =
    Stream.from
      (fun _ ->
         try
           incr charno;
           match Stream.next st with
             | '\n' -> charno := 0; incr lineno; Some '\n'
             | c -> Some c
         with Stream.Failure ->
           None)
  in

  (* Text stream transformed to be parseable *)
  let st =
    braces_of_indent
      (string_of_freeform
         (skip_comment 
            (dos2unix 
               (notab
                 (count_line 
                    (if debug then
                       stream_debugger st
                     else
                       st))))))
  in

  let position () = 
    Printf.sprintf 
      " in file '%s' at line %d, char %d" 
      fn 
      !lineno
      !charno
  in

  (* Lexer for OASIS language *)
  let lexer = 
    make_lexer 
      [ 
        (* Statement *)
        ":"; "if"; "{"; "}"; "else"; "Flag"; "Library"; "Executable"; 
        (* Expression *)
        "!"; "&&"; "||"; "("; ")"; "true"; "false" 
      ]
  in

  (* OASIS expression *)
  let rec parse_factor =
    parser
      | [< 'Kwd "true" >] ->  
          EBool true
      | [< 'Kwd "false" >] ->
          EBool false
      | [< 'Kwd "!"; e = parse_factor >] ->
          ENot e
      | [< 'Kwd "("; e = parse_expr; 'Kwd ")" >] ->
          e
      | [< 'Ident nm; 'Kwd "("; 'Ident vl; 'Kwd ")" >] -> 
          if nm = "flag" then
            EFlag vl
          else
            ETest (nm, vl)
  and parse_term_follow = 
    parser
      | [< 'Kwd "&&"; e1 = parse_factor; e2 = parse_term_follow >] ->
          EAnd (e1, e2)
      | [< >] ->
          EBool true
  and parse_term =
    parser
      | [< e1 = parse_factor; e2 = parse_term_follow >] ->
          EAnd(e1, e2)

  and parse_expr_follow = 
    parser
      | [< 'Kwd "||"; e1 = parse_term; e2 = parse_expr_follow >] ->
          EOr (e1, e2)
      | [< >] ->
          EBool false
  and parse_expr =
    parser
      | [< e1 = parse_term; e2 = parse_expr_follow >] ->
          EOr (e1, e2)
  in

  (* OASIS fields and flags *)
  let rec parse_else =
    parser
      | [< 'Kwd "else"; else_blk = parse_stmt>] ->
          else_blk
      | [< >] ->
          SBlock []
    
  and parse_stmt = 
    parser
      | [<'Ident nm; 'Kwd ":"; 'String str>] ->
          SField(nm, str)

      | [< 'Kwd "if"; e = parse_expr; 
           if_blk = parse_stmt; 
           else_blk = parse_else>] ->
          SIfThenElse(e, if_blk, else_blk)

      | [< 'Kwd "{"; lst = parse_stmt_list; 'Kwd "}">] ->
          SBlock lst

  and parse_stmt_list = 
    parser
      | [< stmt = parse_stmt; tl = parse_stmt_list>] ->
          stmt :: tl
      | [< >] ->
          []
  in

  let rec parse_top_stmt =
    parser
      | [<'Kwd "Flag"; 'Ident nm; flag_blk = parse_stmt>] ->
          TSFlag(nm, flag_blk)

      | [<'Kwd "Library"; 'Ident nm;
           library_blk = parse_stmt>] ->
          TSLibrary (nm, library_blk)

      | [< 'Kwd "Executable"; 'Ident nm; 
           exec_blk = parse_stmt>] ->
          TSExecutable (nm, exec_blk)

      | [< 'Kwd "{"; lst = parse_top_stmt_list; 'Kwd "}">] ->
          TSBlock lst

      | [< stmt = parse_stmt >] ->
          TSStmt stmt

  and parse_top_stmt_list = 
    parser
      | [< top_stmt = parse_top_stmt; tl = parse_top_stmt_list>] ->
          top_stmt :: tl
      | [< >] ->
          []
  in
    (* Main loop *)
    try 
      let st_token =
        lexer st
      in
      let ast = 
        TSBlock (parse_top_stmt_list st_token)
      in
        close_in chn;
        ast
    with 
      | Stream.Error str ->
        (
          if str = "" then 
            failwith ("Syntax error "^(position ()))
          else
            failwith ("Syntax error "^str^(position ()))
        )
      | OASISSchema.MissingField lst ->
          failwith ("Missing fields: "^(String.concat ", " lst))
;;
