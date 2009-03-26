
(** Pack combining several args/checks
    @author Sylvain Le Gall
  *)

module Env = BaseEnvironment;;
module Chk = BaseCheck;;
module Msg = BaseMessage;;

open Unix;;
open FileUtil;;
open FileUtil.StrUtil;;
open FilePath.DefaultPath;;

type package =
    {
      args:     Env.fun_env;
      checks:   Env.fun_env;
      in_files: string list;
      targets:  (string * (Env.env -> unit)) list;
    }
;;

let merge pkg1 pkg2 =
  {
    args     = (fun env -> pkg2.args   (pkg1.args env));
    checks   = (fun env -> pkg2.checks (pkg1.checks env));
    in_files = pkg1.in_files @ pkg2.in_files;
    targets  = pkg1.targets  @ pkg2.targets;
  }
;;

(** Default base package 
  *)
let default = 

  (*
     Base arguments
   *)

  let args env =
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
    let env =
      List.fold_left
        (fun env (name, hlp, dflt) ->
           Env.arg_add 
             (* var name *)
             name
             (* default *)
             dflt
             (* command line argument *)
             [
               "--"^name,
               (fun renv -> Arg.String (fun str -> renv := Env.var_add name str !renv)),
               "dir "^hlp^" ["^dflt^"]"
             ]
             env
        )
        env
        lst
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
    let env = 
      BaseArgExt.wth 
        "build_date"
        "date Date of build"
        (date_R ())
        env
    in

    (* Result *)
      env
  in

  (*
     Base component check
   *)

  (** Return ocaml version and check against a minimal version.
    *)
  let ocaml_version ?min_version env = 
    let ver, nenv =
      Env.cache "ocaml_version"
        (fun env ->
           Msg.checking "ocaml version";
           (Msg.result_wrap Sys.ocaml_version), env
        )
        env
    in
    let nnenv =
      match min_version with 
        | Some ver ->
            Chk.fenv 
              (Chk.version 
                 "ocaml version" 
                 "ocaml" 
                 ver 
                 (fun () -> Sys.ocaml_version)) 
              nenv
        | None ->
            nenv
    in
      ver, nnenv
  in

  (** Check for standard program 
    *)
  let opt_prog prg = Chk.prog_best prg [prg^".opt"; prg]
  in

  let ocamlc     = opt_prog "ocamlc"
  in
  let ocamlopt   = opt_prog "ocamlopt"
  in
  let ocamllex   = opt_prog "ocamllex"
  in
  let ocamlyacc  = opt_prog "ocamlyacc"
  in
  
  let ocamldoc   = Chk.prog "ocamldoc"
  in
  let ocamlfind  = Chk.prog "ocamlfind"
  in

  let camlp4     = Chk.prog "camlp4"
  in
  let mkcamlp4   = Chk.prog "mkcamlp4"
  in
  
  let ocamlmklib = Chk.prog "ocamlmklib"
  in
  
  (** Check what is the best target for platform (opt/byte)
    *)
  let ocamlbest =
    Env.cache "ocamlbest"
      (fun env ->
         try 
           let nenv = 
             Chk.fenv ocamlopt env
           in
             Msg.checking "ocamlbest";
             (Msg.result_wrap "opt"), nenv
         with Not_found ->
           let nenv =
             Chk.fenv ocamlc env
           in
             Msg.checking "ocamlbest";
             (Msg.result_wrap "byte"), nenv
      )
  in
  
  (** Compute the default suffix for link (OS dependent)
    *)
  let suffix_link =
    Env.cache "suffix_link"
      (fun env ->
         Msg.checking "link suffix";
         Msg.result_wrap 
           (match Sys.os_type with 
              | "Win32" -> ".lnk" 
              | _ -> ""
           ),
         env
      )
  in

  (** Compute the default suffix for program (OS dependent)
    *)
  let suffix_program =
    Env.cache "suffix_program"
      (fun env ->
         Msg.checking "program suffix";
         Msg.result_wrap 
           (match Sys.os_type with 
              | "Win32" -> ".exe" 
              | _ -> ""
           ),
         env
      )
  in

  (** Check for everything mandatory for building OCaml project
    *)
  let checks env = 
    Env.chain
      (List.map 
         Chk.fenv
         [
           suffix_link;
           suffix_program;
           ocaml_version;
           ocamlc;
           ocamlbest;
         ]
      )
      env
  in

  (*
    
    Base targets

   *)

  let targets =
    [
      "distclean",
      (fun env ->
         rm ~recurse:true (env.Env.no_dump.Env.fn :: (Env.temporary_get env))
      );

      "configure",
      (fun env ->
         ()
      );

      "print-configure",
      (fun env ->
         if not env.Env.no_dump.Env.print_conf_done then
           ignore(Env.print env)
      );
    ]
  in

  (*
   
    Default base package
   
   *)

    {
      args     = args;
      checks   = checks;
      in_files = [];
      targets  = targets;
    }
;;

