
(** Parse OASIS files using Genlex.
    @author Sylvain Le Gall
  *)

open OASISTypes
open OASISAstTypes
open OASISGettext
open OASISUtils
open Genlex

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

let parse_stream conf st = 

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
      Stack.create ()
    in
    let buffer_rev =
      Stack.create ()
    in

    (* Blank skipping *)
    let rec skip_begin_blank () = 
      match Stream.peek st with 
        | Some ' ' | Some '\t' | Some '\r' ->
            Stream.junk st;
            skip_begin_blank ()
        | _ ->
            ()
    in
    let rec skip_end_blank () = 
      if not (Stack.is_empty buffer_rev) then
        (
          match Stack.pop buffer_rev with 
            | ' ' | '\t' | '\r' ->
                skip_end_blank ()
            | c -> 
                Stack.push c buffer_rev
        )
      else
        ()
    in

    let rec input_line () = 
      let rec input_line_aux () = 
        match Stream.peek st with 
          | Some '\n' ->
              skip_end_blank ();
              Stack.push '\n' buffer_rev;
              Stream.junk st;
              check_continuation ()
          | Some ('"' as c) ->
              Stack.push '\\' buffer_rev;
              Stack.push c buffer_rev;
              Stream.junk st;
              input_line_aux ()
          | Some c ->
              Stack.push c buffer_rev;
              Stream.junk st;
              input_line_aux ()
          | None ->
              skip_end_blank ()
      in
        skip_begin_blank ();
        input_line_aux ()

    and check_continuation () = 
      let continuation_indent = 
        !indent_level + 1
      in
      let begin_new_line =
        Stream.npeek continuation_indent st
      in
      let all_is_blank =
        List.fold_left 
          (fun all_blank c -> all_blank && c = ' ') 
          true
          begin_new_line 
      in
        if all_is_blank then
          input_line ()
    in
    let lookup_freeform () = 
      (* Empty buffer_rev and fill buffer *)
      Stack.push '\n' buffer;
      Stack.push '"' buffer;
      (
        let npeek stk n = 
          let rec npeek_aux acc n = 
            if n > 0 then
              (
                try
                  let c = 
                    Stack.pop stk
                  in
                    npeek_aux (c :: acc) (n - 1)
                with Stack.Empty ->
                  npeek_aux acc 0
              )
            else
              (
                (* Refill stack *)
                List.iter (fun c -> Stack.push c stk) acc;
                List.rev acc
              )
          in
            npeek_aux [] n
        in

        let rec njunk stk n =
          if n > 0 then 
            (
              try 
                ignore (Stack.pop stk);
                njunk stk (n - 1)
              with Stack.Empty ->
                ()
            )
          else
            ()
        in

        let is_blank =
          function 
            | ' ' | '\r' | '\t' -> true
            | _ -> false
        in

          input_line ();
          while not (Stack.is_empty buffer_rev) do 
            match npeek buffer_rev 4 with 
              | '\n' :: '.' :: '\n' :: _ ->
                  njunk buffer_rev 3;
                  Stack.push 'n'  buffer;
                  Stack.push '\\' buffer
              | a :: '\n' :: b :: c :: _ when 
                  not (is_blank a) && 
                  not (is_blank b) && 
                  not (b = '.' && c = '\n') ->
                  njunk buffer_rev 3;
                  Stack.push a   buffer;
                  Stack.push ' ' buffer;
                  Stack.push b   buffer
              | '\n' :: _ ->
                  njunk buffer_rev 1
              | c :: _ ->
                  njunk buffer_rev 1;
                  Stack.push c buffer
              | [] ->
                  ()
          done
      );
      Stack.push '"' buffer
    in
      Stream.from
        (fun _ ->
           try
             if Stack.is_empty buffer then
               (
                 match Stream.next st with
                   | ':' ->
                       lookup_freeform ();
                       Some ':'

                   (* Indent level counting *)
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
                 Some (Stack.pop buffer)
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
                    (if conf.debug then
                       stream_debugger st
                     else
                       st))))))
  in

  let position fmt = 
    Printf.sprintf 
      (f_ "in file '%s' at line %d, char %d")
      (match conf.oasisfn with 
         | Some fn -> fn
         | None -> "<>")
      !lineno
      !charno
  in

  (* Lexer for OASIS language *)
  let lexer = 
    make_lexer 
      [ 
        (* Statement *)
        "+:"; "$:"; ":"; "if"; "{"; "}"; "else"; 
        (* Section *)
        "Flag"; "Library"; "Executable"; 
        "SourceRepository"; "Test";
        "Documentation";
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
            ETest (OASISExpr.expr_test_of_string nm, vl)
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
    
  and parse_field_op = 
    parser
      | [<'Kwd ":"; 'String str>] ->
          FSet str

      | [<'Kwd "+:"; 'String str>] ->
          FAdd str

      | [<'Kwd "$:"; 'String str>] ->
          let e = 
            try
              parse_expr (lexer (Stream.of_string str))
            with e ->
              failwithf3
                (f_ "Error when parsing expresion '%s' %t: %s")
                str
                position
                (Printexc.to_string e)
          in
            FEval e

  and parse_stmt = 
    parser
      | [<'Ident nm; op = parse_field_op>] ->
          SField(nm, op)

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

  let id_or_string =
    parser
      | [<'Ident nm>] ->
          nm
      | [<'String nm>] ->
          nm
  in

  let rec parse_top_stmt =
    parser
      | [<'Kwd "Flag"; nm = id_or_string; flag_blk = parse_stmt>] ->
          TSFlag(nm, flag_blk)

      | [<'Kwd "Library"; nm = id_or_string; library_blk = parse_stmt>] ->
          TSLibrary (nm, library_blk)

      | [< 'Kwd "Executable"; nm = id_or_string; exec_blk = parse_stmt>] ->
          TSExecutable (nm, exec_blk)

      | [< 'Kwd "SourceRepository"; nm = id_or_string; 
                        src_repo_blk = parse_stmt>] ->
          TSSourceRepository (nm, src_repo_blk)

      | [< 'Kwd "Test"; nm = id_or_string; test_blk = parse_stmt>] ->
          TSTest (nm, test_blk)

      | [< 'Kwd "Documentation"; nm = id_or_string; test_blk = parse_stmt>] ->
          TSDocumentation (nm, test_blk)

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
        ast
    with 
      | Stream.Error "" -> 
          failwithf1 (f_ "Syntax error %t") position
      | Stream.Error str ->
          failwithf2 (f_ "Syntax error %s %t") str position
