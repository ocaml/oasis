
open Genlex;;

type name = string
;;

type package_name = string
;;

type url = string
;;

type version = string
;;

type dirname = string
;;

type filename = string
;;

type license = 
    [
        `GPL 
      | `LGPL 
      | `LGPL_link_exn 
      | `BSD3 
      | `BSD4 
      | `PublicDomain 
      | `AllRightsReserved
      | `Other of url
    ]
;;

type dependency = package_name * version option
;;

type lib_info = 
    {
      lib_buildable: bool;
      lib_path:      dirname;
      lib_modules:   string list option;
      lib_extra:     (name * string) list
    }
;;

type exec_info =
    {
      exec_buildable: bool;
      exec_main:      filename;
      exec_extra:     (name * string) list;
    }
;;

type package = 
    {
      name:          package_name;
      license:       license;
      license_file:  filename;
      copyright:     string;
      maintainer:    string;
      author:        string;
      homepage:      url;
      synopsis:      string;
      description:   string;
      category:      url;
      build_depends: dependency list;
      libraries:     lib_info list;
      executables:   exec_info list;
      extra:         (name * string) list;
    }
;;

type env = 
    {
      flag: (name * bool) list;
      test: (name * string) list;
    }
;;

type expr = env -> bool
;;

type stmt = 
  | ASTIfThenElse of expr * stmt_block * stmt_block
  | ASTField of name * string
  | ASTLibrary of stmt_block
  | ASTExecutable of name * stmt_block
  | ASTFlag of name * stmt_block
and stmt_block =
  stmt list
;; 

let parse ?file st = 
  
  (* Case insensitive mem/assoc *)
  let lower_mem nm lst =
    List.mem (String.lowercase nm) lst
  in
  let lower_assoc nm lst =
    List.assoc (String.lowercase nm) lst
  in

  (* Schema *)
  let root_schema =
    [
      "name"; 
      "version"; 
      "copyright"; 
      "license"; 
      "license-file";
      "author"; 
      "maintainer"; 
      "homepage"; 
      "synopsis"; 
      "description";
      "category"; 
      "build-depends"; 
      "build-type";
    ]
  in
  let flag_schema = 
    [
      "description";
      "default";
    ]
  in
  let library_schema =
    [
      "modules";
      "ocaml-flags";
      "build-depends";
    ]
  in
  let executable_schema =
    [
      "main-is";
      "ocaml-flags";
      "build-depends";
    ]
  in

  (* Available test *)
  let test =
    [
      "os";
      "arch";
      "flag";
    ]
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

  (* Record text for debugging *)
  let record_text rlines rflush_line st = 
    let buff =
      Buffer.create 32
    in
    let nst =
      Stream.from
        (fun _ ->
           try 
             (
               match Stream.next st with
                 | '\n' ->
                     rlines := (Buffer.contents buff) :: !rlines;
                     Buffer.clear buff;
                     Some '\n'
                 | c ->
                     Buffer.add_char buff c;
                     Some c
             )
           with Stream.Failure ->
             (
               None
             )
        )
    in
    let flush_line () = 
      try
        while Stream.next nst <> '\n' do 
          ()
        done
      with Stream.Failure ->
        ()
    in
      rflush_line := flush_line;
      nst
  in
  
  (* Rebuild stream with preprocessing *)
  let rlines =
    ref []
  in
  let rflush_line =
    ref (fun () -> ())
  in

  let st =
    braces_of_indent
      (string_of_freeform
         (skip_comment 
            (dos2unix 
               (record_text rlines rflush_line st))))
  in
 
  let position () = 
    let () = 
      !rflush_line ()
    in
    let linenum =
      List.length !rlines
    in
    let line =
      match !rlines with
        | line :: _ ->
           line
        | [] ->
           ""
    in 
      match file with 
        | Some fn ->
            Printf.sprintf " in file '%s' at line %d: %s" fn linenum line
        | None ->
            Printf.sprintf " at line %d: %s" linenum line
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
          fun _ -> true
      | [< 'Kwd "false" >] ->
          fun _ -> false
      | [< 'Kwd "!"; e = parse_factor >] ->
          fun env -> not (e env)
      | [< 'Kwd "("; e = parse_expr; 'Kwd ")" >] ->
          e
      | [< 'Ident nm; 'Kwd "("; 'Ident vl; 'Kwd ")" >] -> 
          (* Check that test exist *)
          if not (lower_mem nm test) then
            failwith ("Unknown test '"^nm^"'"^(position ()));

          if nm = "flag" then
            (
              fun env ->
                (
                  try
                    lower_assoc vl env.flag
                  with Not_found ->
                    failwith ("Undefined flag '"^vl^"'")
                )
            )
          else
            (
              fun env ->
                (
                  let rvl = 
                    lower_assoc nm env.test
                  in
                    rvl = vl
                )
            )

  and parse_term_follow = 
    parser
      | [< 'Kwd "&&"; e1 = parse_factor; e2 = parse_term_follow >] ->
          fun env -> (e1 env) && (e2 env)
      | [< >] ->
          fun _ -> true
  and parse_term =
    parser
      | [< e1 = parse_factor; e2 = parse_term_follow >] ->
          fun env -> (e1 env) && (e2 env)

  and parse_expr_follow = 
    parser
      | [< 'Kwd "||"; e1 = parse_term; e2 = parse_expr_follow >] ->
          fun env -> (e1 env) || (e2 env)
      | [< >] ->
          fun _ -> false 
  and parse_expr =
    parser
      | [< e1 = parse_term; e2 = parse_expr_follow >] ->
          fun env -> (e1 env) || (e2 env)
  in

  (* OASIS fields and flags *)
  let rec parse_else schema = 
    parser
      | [< 'Ident "else"; blk = parse_stmt schema >] ->
          blk
      | [< >] ->
          []

  and parse_stmt schema = 
    parser
      | [<'Ident nm; 'Kwd ":"; 'String str>] ->
          if not (lower_mem nm schema) then
            failwith (Printf.sprintf "Field %S is not authorized" nm);
          [ASTField(nm, str)]

      | [<'Kwd "Flag"; 'Ident nm; flag_blk = parse_stmt flag_schema>] ->
          [ASTFlag(nm, flag_blk)]

      | [<'Kwd "Library"; library_blk = parse_stmt library_schema>] ->
          [ASTLibrary library_blk]

      | [< 'Kwd "Executable"; 'Ident nm; exec_blk = parse_stmt executable_schema>] ->
          [ASTExecutable (nm, exec_blk)]

      | [< 'Kwd "if"; e = parse_expr; 
           if_blk = parse_stmt schema; 
           else_blk = parse_else schema>] ->
          [ASTIfThenElse(e, if_blk, else_blk)]

      | [< 'Kwd "{"; stmt_lst = parse_stmt_list schema; 'Kwd "}">] ->
          stmt_lst

  and parse_stmt_list schema = 
    parser
      | [< stmt = parse_stmt schema; tl = parse_stmt_list schema>] ->
          stmt @ tl
      | [< >] ->
          []
  in
  
  (* Main loop *)
  let st_token =
    lexer st
  in
    try 
      parse_stmt_list root_schema st_token
    with Stream.Error str ->
      (
        if str = "" then 
          failwith ("Syntax error "^(position ()))
        else
          failwith ("Syntax error "^str^(position ()))
      )
;;

let parse_file fn =
  let chn =
    open_in fn
  in
  let res =
    parse ~file:fn (Stream.of_channel chn)
  in
    close_in chn;
    res
;;

let () = 
  let lst = 
    parse_file "test.oasis"
  in
    List.iter
      (function
         | ASTField (nm, vl) -> print_endline (nm^": "^vl)
         | ASTFlag  (nm, _) -> print_endline ("Flag: "^nm)
         | _ -> ()
      )
      lst
;;

