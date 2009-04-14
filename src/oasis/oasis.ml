
type name         = string;;
type package_name = string;;
type url          = string;;
type version      = string;;
type dirname      = string;;
type filename     = string;;

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

type flag = 
    {
      flag_description: string option;
      flag_default:     bool;
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
      conf_type:     string;
      build_type:    string;
      doc_type:      string;
      test_type:     string;
      install_type:  string;
      libraries:     (name * lib) list;
      executables:   (name * exec) list;
      flags:         (name * flag) list;
      extra:         (name * string) list;
    }
;;

type ctxt = 
    {
      oasisfn:      filename;
      srcdir:       filename;
      valid_tests:  name list;
      valid_flags:  name list;
    }
;;

(** Abstract Syntax Tree *)
type expr =
  | ETrue
  | EFalse
  | ENot of expr
  | EAnd of expr * expr
  | EOr of expr * expr
  | EFlag of string
  | ETest of string * string
;;

type stmt =
  | SField of name * string
  | SIfThenElse of expr * stmt * stmt
  | SBlock of stmt list
;;

type top_stmt = 
  | TSLibrary of name * stmt
  | TSExecutable of name * stmt
  | TSFlag of name * stmt
  | TSStmt of stmt
  | TSBlock of top_stmt list
;; 

(** {2 Property list and schema checker} *)

module Schema =
struct

  module SSet = Set.Make(String)

  exception MissingField of name list

  exception UnknownField of name

  type name = string

  type field =
      {
        set: ctxt -> string -> (unit -> unit);
        get: unit -> unit;                     
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

    let set ctxt str =
      (fun () -> v := Some (parse ctxt str))
    in

    let get_default () = 
      match default with
        | Some x ->
            x
        | None ->
            raise (MissingField [name])
    in

    let get t =
      (Hashtbl.find t lname).get ();
      match !v with
        | Some x -> 
            x
        | None -> 
            raise (MissingField [name])
    in
      Hashtbl.replace t lname 
        {
          get = (fun () -> v := Some (get_default ())); 
          set = set
        };
      get

  let set_field t name ctxt str =
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
        {fld with get = fld.set ctxt str}

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

module ValueParser =
struct

  (* Check that string match a Str.regexp *)
  let str_regexp regexp error _ str = 
    if Str.string_match regexp str 0 && 
       (Str.match_beginning ()) = 0 &&
       (Str.match_end ()) = (String.length str) then
        str
    else
      failwith 
        (Printf.sprintf "String '%s' is not a %s" str error)

  (** Check that we have an URL *)
  let url = 
    str_regexp
      (Str.regexp "http://[a-zA-Z0-9\\./]+")
      "URL"

  (** Check that we have a version number *)
  let version =
    str_regexp
      (Str.regexp "[0-9]+\\(\\.[0-9]+\\)*")
      "version"

  (** Check that we a (C) copyright *)
  let copyright _ str =
    if Str.string_match 
         (Str.regexp "\\((c)\\|(C)\\) * [0-9]+\\(-[0-9]+\\)?,? .*") 
         str 0 then
      str
    else
      failwith 
        (Printf.sprintf
           "Copyright must follow the convention \
           '(C) 2008-2009 J.R. Hacker', here it is '%s'"
           str)

  (** String is not empty *)
  let string_not_empty _ str =
    if str <> "" then
      str
    else
      failwith "Expecting not empty string"

  (** File exists *)
  let file_exists ctxt fn = 
    if not (Sys.file_exists (Filename.concat ctxt.srcdir fn)) then
      failwith 
        (Printf.sprintf "File '%s' doesn't exist" fn)
    else
      fn

  (** Directory exists *)
  let directory_exists ctxt fn =
    let rfn =
      Filename.concat ctxt.srcdir fn
    in
      if (Sys.file_exists rfn) && (Sys.is_directory rfn) then
        fn
      else
        failwith 
          (Printf.sprintf "Directory '%s' doesn't exist" fn)

  (** Convert string to boolean *)
  let boolean _ str =
    match String.lowercase str with
      | "true"  -> true
      | "false" -> false
      | _ ->
          failwith 
            (Printf.sprintf 
               "Boolean value must be 'true' \
               or 'false', not '%s'"
               str)

  (** Convert string to build depends *)
  let build_depends _ str =
    let separator =
      Str.regexp " *, *"
    in
    let white_spaces =
      "[ \t]*"
    in
    let not_white_spaces =
      "[^ \t]*"
     in
    let strip_whitespace =
      Str.regexp (white_spaces^"\\("^not_white_spaces^"\\)"^white_spaces)
    in
    let split_version =
      Str.regexp ("\\("^not_white_spaces^"\\)"^
                  white_spaces^
                  "("^white_spaces^"\\(.*\\)"^white_spaces^")")
    in
    let parse_one str =
      if Str.string_match split_version str 0 then
        (Str.matched_group 1 str), 
        Some (Str.matched_group 2 str)
      else if Str.string_match strip_whitespace str 0 then
        (Str.matched_group 1 str),
        None
      else
        str, 
        None
    in
      List.map
        parse_one
        (Str.split separator str)

  (** Convert string to module lists *)
  let modules ctxt str =
    let whitespaces =
      Str.regexp "[ \t]+"
    in
    let lst = 
      Str.split whitespaces str
    in
      List.map 
        (str_regexp 
           (Str.regexp "[A-Z][A-Za-z0-9_]*")
           "module"
           ctxt)
        lst

  (** Convert string to URL *)
  let categories _ str = 
    [str]

  (** Optional value *)
  let opt f ctxt str =
    Some (f ctxt str)
;;
end
;;

module VP = ValueParser
;;

(** {2 Schema} *)

(* Root schema *)
let root_schema, root_gen =
  let schm =
    Schema.create ()
  in
  let name = 
    Schema.new_field schm "name" VP.string_not_empty 
  in
  let version = 
    Schema.new_field schm "version" VP.version
  in
  let license_file =
    Schema.new_field schm "licensefile" VP.file_exists
  in
  let synopsis =
    Schema.new_field schm "synopsis" VP.string_not_empty
  in
  let author =
    Schema.new_field schm "author" VP.string_not_empty
  in
  let license =
    Schema.new_field schm "license"
      (fun ctxt str ->
         match String.uppercase str with 
           | "GPL"   -> `GPL
           | "LGPL"  -> `LGPL
           | "BSD3"  -> `BSD3
           | "BSD4"  -> `BSD4
           | "PUBLICDOMAIN" -> `PublicDomain
           | "LGPL-LINK-EXN" -> `LGPL_link_exn
           | _ -> 
               (try
                  `Other (VP.url ctxt str)
                with _ ->
                  failwith (Printf.sprintf 
                              "'%s' is not an URL or a common license name"
                              str)))
  in
  let conf_type =
    Schema.new_field schm "conftype" 
      ~default:"autobuild"
      VP.string_not_empty
  in
  let build_type =
    Schema.new_field schm "buildtype" 
      ~default:"ocamlbuild"
      VP.string_not_empty
  in
  let doc_type =
    Schema.new_field schm "doctype" 
      ~default:"none"
      VP.string_not_empty
  in
  let test_type =
    Schema.new_field schm "testtype" 
      ~default:"none"
      VP.string_not_empty
  in
  let install_type =
    Schema.new_field schm "installtype"
      ~default:"autobuild"
      VP.string_not_empty
  in
  let copyright =
    Schema.new_field schm "copyright" 
      ~default:None
      (VP.opt VP.copyright)
  in
  let maintainer =
    Schema.new_field schm "maintainer"
      ~default:None
      (VP.opt VP.string_not_empty)
  in
  let homepage =
    Schema.new_field schm "homepage" 
      ~default:None
      (VP.opt VP.url)
  in
  let description =
    Schema.new_field schm "description"
      ~default:None
      (VP.opt VP.string_not_empty)
  in
  let categories =
    Schema.new_field schm "categories"
      ~default:[]
      VP.categories
  in
  let build_depends =
    Schema.new_field schm "builddepends" 
      ~default:[]
      VP.build_depends
  in
    schm,
    (fun tbl libs execs flags ->
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
        conf_type     = conf_type tbl;
        build_type    = build_type tbl;
        doc_type      = doc_type tbl;
        test_type     = test_type tbl;
        install_type  = install_type tbl;
        libraries     = libs;
        executables   = execs;
        flags         = flags;
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
      (VP.opt VP.string_not_empty)
  in
  let default = 
    Schema.new_field schm "default" 
      ~default:true
      VP.boolean
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
    Schema.new_field schm "path" VP.directory_exists
  in
  let buildable = 
    Schema.new_field schm "buildable"
      ~default:true
      VP.boolean
  in
  let modules =
    Schema.new_field schm "modules" 
      ~default:[]
      VP.modules
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
    Schema.new_field schm "mainis" 
      (fun vl ->
         VP.str_regexp
           (Str.regexp ".*\\.ml$")
           ".ml file"
           (VP.file_exists vl))
  in
  let buildable =
    Schema.new_field schm "buildable"
      ~default:true
      VP.boolean
  in
    schm,
    (fun tbl -> 
       {
         exec_buildable = buildable tbl;
         exec_main_is   = main_is tbl;
         exec_extra     = [];
       })
;;

(** Evaluate expression *)
let rec expr_eval ctxt =
  function 
    | ETrue  ->
        true
    | EFalse -> 
        false
    | ENot e -> 
        expr_eval ctxt e 
    | EAnd (e1, e2) ->
        (expr_eval ctxt e1) && (expr_eval ctxt e2)
    | EOr (e1, e2) -> 
        (expr_eval ctxt e1) || (expr_eval ctxt e2)
    | EFlag nm ->
        (
          (* TODO *)
          false
        )
    | ETest (nm, vl) ->
        (
          (* TODO *)
          false
        )
;;

(** Check oasis AST *)
let check valid_tests fn ast = 

  let lowercase_eq str1 str2 =
    (String.lowercase str1) = (String.lowercase str2)
  in

  (* Check AST *)
  let rec check_expr ctxt =
    function
      | ETrue | EFalse -> 
          ()
      | ENot e -> 
          check_expr ctxt e 
      | EAnd (e1, e2) | EOr (e1, e2) -> 
          check_expr ctxt e1; 
          check_expr ctxt e2
      | EFlag nm ->
          (
            if not (List.exists (lowercase_eq nm) ctxt.valid_flags) then
              failwith 
                (Printf.sprintf 
                   "Unknown flag '%s'"
                   nm)
          )
      | ETest (nm, vl) ->
          (
            if not (List.exists (lowercase_eq nm) ctxt.valid_tests) then
              failwith 
                (Printf.sprintf 
                   "Unknown test '%s'"
                   nm)
          )
  in

  let rec check_stmt wrtr ctxt =
    function
      | SField (nm, str) -> 
          Schema.set_field wrtr nm ctxt str
      | SIfThenElse (e, blk1, blk2) ->
          check_expr ctxt e;
          check_stmt wrtr ctxt blk1;
          check_stmt wrtr ctxt blk2
      | SBlock blk ->
          List.iter (check_stmt wrtr ctxt) blk
  in

  let rec check_top_stmt root_wrtr ctxt =
    function
      | TSFlag (nm, blk) -> 
          let ctxt = 
            check_schema (Schema.writer flag_schema) ctxt blk
          in
            {ctxt with valid_flags = nm :: ctxt.valid_flags}
      | TSLibrary (_, blk) -> 
          check_schema (Schema.writer lib_schema) ctxt blk
      | TSExecutable (_, blk) ->
          check_schema (Schema.writer exec_schema) ctxt blk
      | TSStmt stmt ->
          check_stmt root_wrtr ctxt stmt; 
          ctxt
      | TSBlock lst ->
          List.fold_left 
            (check_top_stmt root_wrtr) 
            ctxt
            lst
  and check_schema schm ctxt blk =
    let wrtr =
      Schema.writer schm
    in
      check_stmt wrtr ctxt blk;
      Schema.check wrtr;
      ctxt
  in

    check_top_stmt 
      root_schema 
      {
        oasisfn     = fn;
        srcdir      = Filename.dirname fn;
        valid_flags = [];
        valid_tests = valid_tests;
      }
      ast
;;

let oasis (ast, ctxt) =
  let rec oasis_stmt wrtr =
    function
      | SField (nm, str) -> 
          Schema.set_field wrtr nm ctxt str

      | SIfThenElse (e, blk1, blk2) -> 
          let blk =
            if expr_eval ctxt e then
              blk1 
            else
              blk2
          in
            oasis_stmt wrtr blk

      | SBlock blk ->
          List.iter (oasis_stmt wrtr) blk
  in

  let rec oasis_top_stmt root_wrtr ((libs, execs, flags) as acc) =
    function
      | TSFlag (nm, blk) -> 
          let wrtr =
            Schema.writer flag_schema 
          in
          let flag =
            oasis_stmt wrtr blk;
            flag_gen wrtr
          in
            libs, execs, (nm, flag) :: flags

      | TSLibrary (nm, blk) -> 
          let wrtr =
            Schema.writer lib_schema
          in
          let lib = 
            oasis_stmt wrtr blk;
            lib_gen wrtr
          in
            ((nm, lib) :: libs), execs, flags

      | TSExecutable (nm, blk) -> 
          let wrtr =
            Schema.writer exec_schema
          in
          let exec = 
            oasis_stmt wrtr blk;
            exec_gen wrtr
          in
            libs, ((nm, exec) :: execs), flags

      | TSStmt stmt -> 
          oasis_stmt root_wrtr stmt;
          acc

      | TSBlock blk -> 
          List.fold_left (oasis_top_stmt root_wrtr) acc blk
  in
  let wrtr =
    Schema.writer root_schema 
  in
  let libs, execs, flags =
    oasis_top_stmt wrtr ([], [], []) ast
  in
    root_gen wrtr libs execs flags
;;
