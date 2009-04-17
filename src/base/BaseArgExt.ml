
(** Handle command line argument
    @author Sylvain Le Gall
  *)

open BaseEnvironment;;
open Unix;;

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

let enable name hlp default renv =
  let arg_name =
    tr_arg name
  in
  let default_str =
    if default then "true" else "false"
  in
    renv := var_set
              name
              default_str
              (var_define 
                 name 
                 (fun env -> default_str, env)
                 !renv);
    [
      "--enable-"^arg_name,
      Arg.Unit (fun () -> renv := var_set name "true" !renv),
      " Enable "^hlp^(if default then " [default]" else "");

      "--disable-"^arg_name,
      Arg.Unit (fun () -> renv := var_set name "false" !renv),
      " Disable "^hlp^(if not default then " [default]" else "");
    ]
;;
 
let wth name hlp default renv =
  renv := var_set
            name
            default
            (var_define name 
               (var_expand default) 
               !renv);
  [
    "--with-"^(tr_arg name),
    Arg.String (fun str -> renv := var_set name str !renv),
    hlp^" ["^default^"]"
  ]
;;

let parse argv args env =
    (* Simulate command line for Arg *)
    let current =
      ref 0
    in

    let renv = 
      ref env
    in
    let args =
      List.flatten
        (List.map 
           (fun fargs -> fargs renv)
           args)
    in
      (
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
      );
      !renv
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
  (* Build date argument *)
  let date_R () = 
    let string_of_mon i = 
      [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; 
        "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; 
        "Nov"; "Dec" |].(i)
    in
    let string_of_wday i = 
      [| "Sun"; "Mon"; "Thu"; "Wed"; "Tue"; 
        "Fri"; "Sat" |].(i)
    in
    let tm =
      gmtime (time ())
    in
      Printf.sprintf 
        "%s, %02d %s %d %02d:%02d:%02d +0000" 
        (string_of_wday tm.tm_wday)
        tm.tm_mday 
        (string_of_mon tm.tm_mon) 
        (1900 + tm.tm_year) 
        tm.tm_hour 
        tm.tm_min 
        tm.tm_sec
  in
  let arg_date = 
    wth 
      "build_date"
      "date Date of build"
      (date_R ())
  in

    fun renv ->
      List.fold_left
        (fun acc (name, hlp, dflt) ->
           renv := var_define name (var_expand dflt) !renv;
           (
             "--"^name,
             Arg.String (fun str -> renv := var_set name str !renv),
             "dir "^hlp^" ["^dflt^"]"
           ) :: acc
        )
        ((arg_date renv) @ (BaseEnvironment.args renv))
        lst
;;
