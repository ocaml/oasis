
open Genlex;;

type name         = string;;
type package_name = string;;
type url          = string;;
type version      = string;;
type dirname      = string;;
type filename     = string;;
type build_type   = string;;

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

type lib = 
    {
      lib_buildable: bool;
      lib_path:      dirname;
      lib_modules:   string list;
      lib_extra:     (name * string) list
    }
;;

type exec =
    {
      exec_buildable: bool;
      exec_main_is:   filename;
      exec_extra:     (name * string) list;
    }
;;

type package = 
    {
      name:          package_name;
      version:       version;
      license:       license;
      license_file:  filename;
      copyright:     string option;
      maintainer:    string option;
      author:        string;
      homepage:      url option;
      synopsis:      string;
      description:   string option;
      categories:    url list;
      build_depends: dependency list;
      build_type:    build_type;
      libraries:     lib list;
      executables:   exec list;
      extra:         (name * string) list;
    }
;;

type flag = 
    {
      flag_description: string option;
      flag_default:     bool;
    }
;;

type env = 
    {
      flag: (name * bool) list;
      test: (name * string) list;
    }
;;

(** Abstract Syntax Tree *)
type expr = env -> bool
;;

type stmt = 
  | ASTIfThenElse of expr * stmt * stmt
  | ASTBlock of stmt list
  | ASTField of name * string
  | ASTLibrary of stmt
  | ASTExecutable of name * stmt
  | ASTFlag of name * stmt
and stmt_block =
  stmt list
;; 

module Schema =
struct

  module SSet = Set.Make(String)

  exception MissingField of name list

  exception UnknownField of name

  type name = string

  type field =
      {
        set: string -> (unit -> unit);
        get:   unit -> unit;                     
      }

  type t = (name, field) Hashtbl.t

  let create () = 
    Hashtbl.create 13

  let new_field t name ?default parse =
    let lname =
      String.lowercase name
    in
    let v = 
      ref None 
    in

    let set str =
      (fun () -> v := Some (parse str))
    in

    let get_default () = 
      match default with
        | Some x ->
            x
        | None ->
            raise (MissingField [lname])
    in

    let get t =
      (Hashtbl.find t lname).get ();
      match !v with
        | Some x -> 
            x
        | None -> 
            raise (MissingField [lname])
    in
      Hashtbl.replace t lname 
        {
          get = (fun () -> v := Some (get_default ())); 
          set = set
        };
      get

  let set_field t name str =
    let lname =
      String.lowercase name
    in
    let fld =
      try
        Hashtbl.find 
          t 
          lname
      with Not_found ->
        raise (UnknownField name)
    in
      Hashtbl.replace 
        t
        lname 
        {fld with get = fld.set str}

  let check t =
    let msgfld =
      Hashtbl.fold
        (fun nm fld msgfld -> 
           try
             fld.get ();
             msgfld
           with 
             | MissingField [hd] ->
                 hd :: msgfld 
             | MissingField lst ->
                 lst @ msgfld)
        t
        []
    in
      if msgfld <> [] then
        raise (MissingField msgfld)

  let writer =
    Hashtbl.copy

  (* TODO:
   * X-field
  let extra_options nm str =
    if (String.length str) > 0 && (str.[0] = 'x' || str.[0] = 'X') then
      (String.lowercase nm), str
    else
      failwith 
        (Printf.sprintf "Unrecognized option '%s'" nm)
  in

   *)
end
;;

(** {2 Schema} *)

(**/**)
let basedir =
  ref "."
;;

(* Provide string match a Str.regexp *)
let match_str regexp error str = 
  if Str.string_match regexp str 0 && 
     (Str.match_beginning ()) = 0 &&
     (Str.match_end ()) = (String.length str) then
      str
  else
    failwith 
      (Printf.sprintf "String '%s' is not a %s" str error)
;;

(** Check that we have an URL *)
let match_url = 
  match_str
    (Str.regexp "http://[a-zA-Z0-9\\./]+")
    "URL"
;;

(** Check that we have a version number *)
let match_version =
  match_str
    (Str.regexp "[0-9]+\\(\\.[0-9]+\\)*")
    "version"
;;

(** Check that we a (C) copyright *)
let match_copyright str =
  if Str.string_match 
       (Str.regexp "\\((c)|(C)\\) * [0-9]+\\(-[0-9]+\\)? .*") 
       str 0 then
    str
  else
    failwith 
      (Printf.sprintf
         "Copyright must follow the convention 
         '(C) 2008-2009 J.R. Hacker', here it is '%s"
         str)
;;

(** String is not empty *)
let string_not_empty str =
  if str <> "" then
    str
  else
    failwith "Expecting not empty string"
;;

(** File exists *)
let file_exists fn = 
  if not (Sys.file_exists (Filename.concat !basedir fn)) then
    failwith 
      (Printf.sprintf "File '%s' doesn't exist" fn)
  else
    fn
;;

(** Directory exists *)
let directory_exists fn =
  let rfn =
    Filename.concat !basedir fn
  in
    if (Sys.file_exists rfn) && (Sys.is_directory rfn) then
      fn
    else
      failwith 
        (Printf.sprintf "Directory '%s' doesn't exist" fn)
;;

(** Convert string to boolean *)
let parse_bool str =
  match String.lowercase str with
    | "true"  -> true
    | "false" -> false
    | _ ->
        failwith 
          (Printf.sprintf 
             "Boolean value must be 'true' 
             or 'false', not '%s'"
             str)
;;

(** Convert string to build depends *)
let parse_build_depends str =
  (* TODO *)
  [str, None]
;;

(** Convert string to module lists *)
let match_modules str =
  (* TODO *)
  [str]
;;

(** Convert string to URL *)
let urls str = 
  (* TODO *)
  [str]
;;

(** Optional value *)
let opt f =
  fun str -> Some (f str)
;;
(**/**)

(* Root schema *)
let root_schema, root_gen =
  let schm =
    Schema.create ()
  in
  let name = 
    Schema.new_field schm "name" string_not_empty 
  in
  let version = 
    Schema.new_field schm "version" match_version
  in
  let license_file =
    Schema.new_field schm "licensefile" file_exists
  in
  let synopsis =
    Schema.new_field schm "synopsis" string_not_empty
  in
  let build_type =
    Schema.new_field schm "buildtype" string_not_empty
  in
  let author =
    Schema.new_field schm "author" 
      string_not_empty
  in
  let license =
    Schema.new_field schm "license"
      (fun str ->
         match String.uppercase str with 
           | "GPL"   -> `GPL
           | "LGPL"  -> `LGPL
           | "BSD3"  -> `BSD3
           | "BSD4"  -> `BSD4
           | "PublicDomain" -> `PublicDomain
           | "LGPL-link-exn" -> `LGPL_link_exn
           | str -> 
               (try
                  `Other (match_url str)
                with _ ->
                  failwith (Printf.sprintf 
                              "'%s' is not an URL or a common license name"
                              str)))
  in
  let copyright =
    Schema.new_field schm "copyright" 
      ~default:None
      (opt match_copyright)
  in
  let maintainer =
    Schema.new_field schm "maintainer"
      ~default:None
      (opt string_not_empty)
  in
  let homepage =
    Schema.new_field schm "homepage" 
      ~default:None
      (opt match_url)
  in
  let description =
    Schema.new_field schm "description"
      ~default:None
      (opt string_not_empty)
  in
  let categories =
    Schema.new_field schm "categories"
      ~default:[]
      urls
  in
  let build_depends =
    Schema.new_field schm "builddepends" 
      ~default:[]
      parse_build_depends
  in
    schm,
    (fun tbl ->
      {
        name          = name tbl;
        version       = version tbl;
        license       = license tbl;
        license_file  = license_file tbl;
        copyright     = copyright tbl;
        maintainer    = maintainer tbl;
        author        = author tbl;
        homepage      = homepage tbl;
        synopsis      = synopsis tbl;
        description   = description tbl;
        categories    = categories tbl;
        build_depends = build_depends tbl;
        build_type    = build_type tbl;
        libraries     = [];
        executables   = [];
        extra         = [];
      })
;;

(** Flag schema and generator *)
let flag_schema, flag_gen = 
  let schm =
    Schema.create ()
  in
  let descr = 
    Schema.new_field schm "description" 
      ~default:None 
      (opt string_not_empty)
  in
  let default = 
    Schema.new_field schm "default" 
      ~default:true
      parse_bool
  in
    schm,
    (fun tbl ->
       {
         flag_description = descr tbl;
         flag_default     = default tbl;
       })
;;

(** Library schema and generator *)
let lib_schema, lib_gen =
  let schm =
    Schema.create ()
  in
  let path =
    Schema.new_field schm "path" directory_exists
  in
  let buildable = 
    Schema.new_field schm "buildable"
      ~default:true
      parse_bool
  in
  let modules =
    Schema.new_field schm "modules" 
      ~default:[]
      match_modules
  in
    schm,
    (fun tbl ->
       {
         lib_buildable = buildable tbl;
         lib_path      = path tbl;
         lib_modules   = modules tbl;
         lib_extra     = [];
       })
;;

(** Executable schema and generator *)
let exec_schema, exec_gen =
  let schm =
    Schema.create ()
  in
  let main_is =
    Schema.new_field schm "mainis" file_exists
  in
  let buildable =
    Schema.new_field schm "buildable"
      ~default:true
      parse_bool
  in
    schm,
    (fun tbl -> 
       {
         exec_buildable = buildable tbl;
         exec_main_is   = main_is tbl;
         exec_extra     = [];
       })
;;

(** {2 Test} *)

(* Available test *)
let tests =
  let rec find nm lst =
    match lst with 
      | nm_check :: vl :: _ when nm_check = nm ->
          vl
      | _ :: tl ->
          find nm tl
      | [] ->
          raise Not_found
  in
    [
      "os_type",      find "os_type:";
      "system",       find "system:";
      "architecture", find "architecture:";
      "cc",           find "native_c_compiler:";
    ]
;;

let env_of_ocamlc ocamlc =
  let tests = 
    let chn =
      Unix.open_process_in (ocamlc^" -config")
    in
    let lst =
      ref []
    in
    let buff =
      Buffer.create 0
    in
      (
        try
          while true do
            match input_char chn with
              | ' ' | '\r' | '\n' -> 
                if Buffer.length buff > 0 then
                  (
                    lst := Buffer.contents buff :: !lst;
                    Buffer.clear buff
                  )
              | c ->
                  Buffer.add_char buff c
          done
        with End_of_file ->
          ()
      );
      (
        match Unix.close_process_in chn with
          | Unix.WEXITED 0 -> 
              ()
          | _ -> 
              failwith 
                (Printf.sprintf 
                   "Failed running '%s -config'" 
                   ocamlc)
      );
      lst := List.rev !lst;
      List.rev_map
        (fun (nm, f) -> 
           try 
             nm, f !lst
           with Not_found ->
             failwith 
               (Printf.sprintf 
                  "Field '%s' not found in 'ocamlc -config' output"
                  nm))
        tests
  in
    {
      flag = [];
      test = tests;
    }
;;

(** Parsing *)
let parse_file fn = 
  let chn =
    open_in fn
  in

  let st =
    Stream.of_channel chn
  in

  let () = 
    basedir := Filename.dirname fn
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
 
  let position ?(prev=false) () = 
    let () = 
      !rflush_line ()
    in
    let lines = 
      if prev then
        (match !rlines with 
           | _ :: lines -> lines
           | [] -> [])
      else
        !rlines
    in
    let linenum =
      List.length lines
    in
    let line =
      match lines with
        | line :: _ ->
           line
        | [] ->
           ""
    in 
      Printf.sprintf " in file '%s' at line %d: %s" fn linenum line
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
          let lnm =
            String.lowercase nm
          in
            (
              try
                if lnm = "flag" then
                  ()
                else
                  let _t : (string list -> string) = 
                    List.assoc lnm tests
                  in
                    ()
              with Not_found ->
                failwith ("Unknown test '"^nm^"'"^(position ()))
            );

            if nm = "flag" then
              (
                fun env ->
                  (
                    try
                      List.assoc (String.lowercase vl) env.flag
                    with Not_found ->
                      failwith ("Undefined flag '"^vl^"'")
                  )
              )
            else
              (
                fun env ->
                  (
                    let rvl = 
                      List.assoc lnm env.test
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
  let rec parse_else = 
    parser
      | [< 'Ident "else"; blk = parse_stmt >] ->
          blk
      | [< >] ->
          ASTBlock []

  and parse_stmt = 
    parser
      | [<'Ident nm; 'Kwd ":"; 'String str>] ->
          ASTField(nm, str)

      | [<'Kwd "Flag"; 'Ident nm; flag_blk = parse_stmt>] ->
          ASTFlag(nm, flag_blk)

      | [<'Kwd "Library"; 
           library_blk = parse_stmt>] ->
          ASTLibrary library_blk

      | [< 'Kwd "Executable"; 'Ident nm; 
           exec_blk = parse_stmt>] ->
          ASTExecutable (nm, exec_blk)

      | [< 'Kwd "if"; e = parse_expr; 
           if_blk = parse_stmt; 
           else_blk = parse_else>] ->
          ASTIfThenElse(e, if_blk, else_blk)

      | [< 'Kwd "{"; stmt_lst = parse_stmt_list; 'Kwd "}">] ->
          ASTBlock stmt_lst

  and parse_stmt_list = 
    parser
      | [< stmt = parse_stmt; tl = parse_stmt_list>] ->
          stmt :: tl
      | [< >] ->
          []
  in

  (* Check AST *)
  let check ast = 
    let rec check_stmt wrtr =
      function
        | ASTField (nm, str) -> 
            Schema.set_field wrtr nm str
        | ASTFlag (_, blk) -> 
            check_schema (Schema.writer flag_schema) blk
        | ASTLibrary blk -> 
            check_schema (Schema.writer lib_schema)  blk
        | ASTExecutable (_, blk) ->
            check_schema (Schema.writer exec_schema) blk
        | ASTIfThenElse (_, blk1, blk2) ->
            check_stmt wrtr blk1;
            check_stmt wrtr blk2
        | ASTBlock lst ->
            List.iter (check_stmt wrtr) lst
    and check_schema schm blk =
      let wrtr =
        Schema.writer schm
      in
        check_stmt wrtr blk;
        Schema.check wrtr
    in
      check_schema root_schema ast
  in
  
  (* Main loop *)
  let st_token =
    lexer st
  in
    try 
      let ast = 
        ASTBlock (parse_stmt_list st_token)
      in
        close_in chn;
        check ast;
        ast
    with 
      | Stream.Error str ->
        (
          if str = "" then 
            failwith ("Syntax error "^(position ()))
          else
            failwith ("Syntax error "^str^(position ()))
        )
      | Schema.MissingField lst ->
          failwith ("Missing fields: "^(String.concat ", " lst))
;;


let flags env ast =
  let rec flags_rec fwrtr acc =
    function
      | ASTField (nm, str) -> 
          fwrtr nm str; acc
      | ASTLibrary blk -> 
          flags_rec fwrtr acc blk
      | ASTExecutable (_, blk) -> 
          flags_rec fwrtr acc blk
      | ASTIfThenElse (tst, blk1, blk2) -> 
          flags_rec fwrtr acc (if tst env then blk1 else blk2)
      | ASTBlock blk -> 
          List.fold_left (flags_rec fwrtr) acc blk
      | ASTFlag (nm, blk)  -> 
          let wrtr =
            Schema.writer flag_schema 
          in
          let (env, lst) = 
            flags_rec (Schema.set_field wrtr) acc blk
          in
          let flag =
            flag_gen wrtr
          in
            {env with flag = (nm, flag.flag_default) :: env.flag},
            ((nm, flag) :: lst)
  in
  let (_, lst) = 
    flags_rec (fun _ _ -> ()) (env, []) ast
  in
    lst
;;

let oasis env ast =
  let rec oasis_rec wrtr acc =
    function
      | ASTField (nm, str) -> 
          Schema.set_field wrtr nm str;
          acc

      | ASTFlag (_, blk) -> 
          oasis_rec wrtr acc blk

      | ASTLibrary blk -> 
          let wrtr =
            Schema.writer lib_schema
          in
          let (libs, execs) = 
            oasis_rec wrtr acc blk
          in
            ((lib_gen wrtr) :: libs), execs

      | ASTExecutable (nm, blk) -> 
          let wrtr =
            Schema.writer exec_schema
          in
          let (libs, execs) = 
            oasis_rec wrtr acc blk
          in
            libs, ((exec_gen wrtr) :: execs)

      | ASTIfThenElse (tst, blk1, blk2) -> 
          oasis_rec wrtr acc 
            (if tst env then 
               blk1 
             else 
               blk2)

      | ASTBlock blk -> 
          List.fold_left (oasis_rec wrtr) acc blk
  in
  let wrtr =
    Schema.writer root_schema 
  in
  let libs, execs =
    oasis_rec wrtr ([], []) ast
  in
    {(root_gen wrtr) with libraries = libs; executables = execs}
;;
