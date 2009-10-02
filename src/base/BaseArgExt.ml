
(** Handle command line argument
    @author Sylvain Le Gall
  *)

open BaseEnvironment;;

let tr_arg str =
  let buff =
    Buffer.create (String.length str)
  in
    String.iter 
      (function 
         | '_' | ' ' | '\n' | '\r' | '\t' -> Buffer.add_char buff '-'
         | c -> Buffer.add_char buff c
      )
      str;
    Buffer.contents buff
;;

let enable name hlp default_choices env =
  let arg_name =
    tr_arg name
  in
  let default = 
    BaseExpr.choose default_choices env
  in
    var_set 
      name 
      (lazy (if default then "true" else "false"))
      env;
    [
      "--enable-"^arg_name,
      Arg.Unit (fun () -> var_set name (lazy "true") env),
      " Enable "^hlp^(if default then " [default]" else "");

      "--disable-"^arg_name,
      Arg.Unit (fun () -> var_set name (lazy "false") env),
      " Disable "^hlp^(if not default then " [default]" else "");
    ]
;;
 
let wth name hlp default env =
    var_set name (lazy default) env; 
    [
      "--with-"^(tr_arg name),
      Arg.String (fun str -> var_set name (lazy str) env),
      hlp^" ["^default^"]"
    ]
;;

let parse argv args env =
    (* Simulate command line for Arg *)
    let current =
      ref 0
    in

    let args =
      List.flatten
        (List.map 
           (fun fargs -> fargs env)
           args)
    in
      try
        Arg.parse_argv
          ~current:current
          (Array.concat [[|"none"|]; argv])
          (Arg.align args)
          (fun str -> 
             failwith 
               ("Don't know what to do with arguments: '"^str^"'"))
          "configure options:"
      with Arg.Help txt | Arg.Bad txt ->
        BaseMessage.error txt
;;

let default =
  (* Standard paths *)
  let lst =
    [
      "prefix",
      "install architecture-independent files dir",
      (match Sys.os_type with
         | "Win32" ->
             "%PROGRAMFILES%\\$pkg_name"
         | _ ->
             "/usr/local"
      );
      
      "eprefix",
      "Install architecture-dependent files in dir",
      "$prefix";

      "bindir",
      "User executables",
      Filename.concat "$eprefix" "bin";

      "sbindir",
      "System admin executables",
      Filename.concat "$eprefix" "sbin";

      "libexecdir",
      "Program executables",
      Filename.concat "$eprefix" "libexec";

      "sysconfdir",
      "Read-only single-machine data",
      Filename.concat "$prefix" "etc";

      "sharedstatedir",
      "Modifiable architecture-independent data",
      Filename.concat "$prefix" "com";

      "localstatedir",
      "Modifiable single-machine data",
      Filename.concat "$prefix" "var";

      "libdir",
      "Object code libraries",
      Filename.concat "$eprefix" "lib";

      "datarootdir",
      "Read-only arch.-independent data root",
      Filename.concat "$prefix" "share";

      "datadir",
      "Read-only architecture-independent data",
      "$datarootdir";

      "infodir",
      "Info documentation",
      Filename.concat "$datarootdir" "info";

      "localedir",
      "Locale-dependent data",
      Filename.concat "$datarootdir" "locale";

      "mandir",
      "Man documentation",
      Filename.concat "$datarootdir" "man";

      "docdir",
      "Documentation root",
      Filename.concat (Filename.concat "$datarootdir" "doc") "$pkg_name";

      "htmldir",
      "HTML documentation",
      "$docdir";

      "dvidir",
      "DVI documentation",
      "$docdir";

      "pdfdir",
      "PDF documentation",
      "$docdir";

      "psdir",
      "PS documentation",
      "$docdir";
    ]
  in
    fun env ->
      List.fold_left
        (fun acc (name, hlp, dflt) ->
           var_set name (lazy dflt) env;
           (
             "--"^name,
             Arg.String (fun str -> var_set name (lazy str) env),
             "dir "^hlp^" ["^dflt^"]"
           ) :: acc
        )
        (BaseEnvironment.args env)
        lst
;;

let prefix         = var_get "prefix"
let eprefix        = var_get "eprefix"
let bindir         = var_get "bindir"
let sbindir        = var_get "sbindir"
let libexecdir     = var_get "libexecdir"
let sysconfdir     = var_get "sysconfdir"
let sharedstatedir = var_get "sharedstatedir"
let localstatedir  = var_get "localstatedir"
let libdir         = var_get "libdir"
let datarootdir    = var_get "datarootdir"
let datadir        = var_get "datadir"
let infodir        = var_get "infodir"
let localedir      = var_get "localedir"
let mandir         = var_get "mandir"
let docdir         = var_get "docdir"
let htmldir        = var_get "htmldir"
let dvidir         = var_get "dvidir"
let pdfdir         = var_get "pdfdir"
let psdir          = var_get "psdir"
