
open Genlex;;

type name = string
;;

type value = 
  | VFreeform of string
;;

type stmt = 
  | SField of name * value
  | SFlag of name * stmt_block
and stmt_block =
 stmt list
;; 

type oasis =
  stmt list
;;

let parse ?file st = 
  
  (* Available test *)
  let test =
    [
      "os", "linux";
      "arch", "i386";
    ]
  in

  (* Available flags *)
  let flag = 
    [
    ]
  in

  (* Get rid of MS-DOS file format *)
  let st = 
    Stream.from
      (fun _ ->
         (
           match Stream.npeek 2 st with 
             | ['\r'; '\n'] ->
                 Stream.junk st
             | _ ->
                 ()
         );
         try
           Some (Stream.next st)
         with Stream.Failure ->
           None
      )
  in

  (* Counting line *)
  let rline =
    ref 1
  in
  let st = 
    Stream.from
      (fun _ ->
         try 
           (
             match Stream.next st with
               | '\n' -> incr rline; Some '\n'
               | c -> Some c
           )
         with Stream.Failure ->
           None
      )
  in

  let position () = 
    match file with 
      | Some fn ->
          " in file '"^fn^"' at line "^(string_of_int !rline)
      | None ->
          " at line "^(string_of_int !rline)
  in

  (* Skip comment *)
  let st =
    let rec skip_line () =
      if (Stream.next st) <> '\n' then
        skip_line ()
    in
    let rec skip_comment () = 
      match Stream.next st with
        | '#' -> skip_line (); skip_comment ()
        | c -> c
    in
      Stream.from
        (fun _ ->
           try 
             Some (skip_comment ())
           with Stream.Failure ->
             None
        )
  in

  (* Parsing field value, with line continuation *)
  let parse_field_value _ = 
    let buff =
      Buffer.create 1024
    in
    let rec skip_blank () = 
      match Stream.peek st with 
        | Some ' ' | Some '\t' | Some '\r' ->
            Stream.junk st;
            skip_blank ()
        | _ ->
            ()
    in
    let rec parse_field_value_aux () = 
      match Stream.npeek 4 st with 
        | [ '\n'; ' ' ; '.' ; '\n' ] ->
            Stream.junk st; Stream.junk st;
            Stream.junk st; 
            Buffer.add_string buff "\n";
            parse_field_value_aux ()

        | '\n' :: ' ' :: _ ->
            Stream.junk st; Stream.junk st;
            Buffer.add_char buff '\n';
            parse_field_value_aux ()

        | '\n' :: _ ->
            Stream.junk st;
            Buffer.contents buff

        | c :: _ ->
            Stream.junk st;
            Buffer.add_char buff c;
            parse_field_value_aux ()

        | [] ->
            Buffer.contents buff 
    in
      skip_blank ();
      parse_field_value_aux ()
  in 
  
  (* Lexer for OASIS language *)
  let lexer = 
    make_lexer 
      [ 
        (* Statement *)
        ":"; "if"; "{"; "}"; "else"; "Flag"; 
        (* Expression *)
        "!"; "&&"; "||"; "("; ")"; "true"; "false" 
      ]
  in

  (* OASIS expression *)
  let rec parse_factor =
    parser
      | [< 'Kwd "true" >] ->  
          true
      | [< 'Kwd "false" >] ->
          false
      | [< 'Kwd "!"; e = parse_factor >] ->
          not e
      | [< 'Kwd "("; e = parse_expr; 'Kwd ")" >] ->
          e
      | [< 'Ident nm; 'Kwd "("; 'Ident vl; 'Kwd ")" >] -> 
          if nm = "flag" then
            (
              try
                List.assoc vl flag
              with Not_found ->
                failwith ("Undefined flag '"^vl^"'"^(position ()))
            )
          else
            (
              try
                let rvl = 
                  List.assoc nm test
                in
                  rvl = vl
              with Not_found ->
                failwith ("Unknown test '"^nm^"'"^(position ()))
            )

  and parse_term_follow = 
    parser
      | [< 'Kwd "&&"; e1 = parse_factor; e2 = parse_term_follow >] ->
          e1 && e2
      | [< >] ->
          true
  and parse_term =
    parser
      | [< e1 = parse_factor; e2 = parse_term_follow >] ->
          e1 && e2

  and parse_expr_follow = 
    parser
      | [< 'Kwd "||"; e1 = parse_term; e2 = parse_expr_follow >] ->
          e1 || e2
      | [< >] ->
         false 
  and parse_expr =
    parser
      | [< e1 = parse_term; e2 = parse_expr_follow >] ->
          e1 || e2
  in

  (* OASIS fields and flags *)
  let rec parse_blk =
    parser
      | [< 'Kwd "{"; lst = parse_oasis; 'Kwd "}" >] ->
          lst

  and parse_else = 
    parser
      | [< 'Ident "else"; blk = parse_blk >] ->
          blk
      | [< >] ->
          []

  and parse_stmt = 
    parser
      | [< 'Ident nm; 'Kwd ":"; vl = parse_field_value >] ->
          [SField(nm, VFreeform vl)]
      | [< 'Kwd "if"; e = parse_expr; if_blk = parse_blk; else_blk = parse_else >] ->
          if e then if_blk else else_blk
      | [< 'Kwd "Flag"; 'Ident nm; flag_blk = parse_blk >] ->
          [SFlag(nm, flag_blk)]

  and parse_oasis = 
    parser
      | [< stmt = parse_stmt; tl = parse_oasis >] ->
          stmt @ tl
      | [< >] ->
          []
  in
  
  (* Main loop *)
  let st_token =
    lexer st
  in
    try 
      parse_oasis st_token
    with Stream.Error str ->
      (
        if str = "" then 
          failwith ("Syntax error"^(position ()))
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
         | SField (nm, VFreeform vl) -> print_endline (nm^": "^vl)
         | SFlag  (nm, _) -> print_endline ("Flag: "^nm)
      )
      lst
;;

