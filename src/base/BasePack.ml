
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
      args:     Env.env ref -> (string * Arg.spec * string) list;
      checks:   Env.fun_env;
      in_files: string list;
    }
;;

let merge lst =
  {
    args     = BaseArgExt.merge (List.map (fun pkg -> pkg.args) lst);
    checks   = List.fold_left (fun f pkg env -> f (pkg.checks env)) (fun x -> x) lst;
    in_files = List.flatten (List.map (fun pkg -> pkg.in_files) lst);
  }
;;

(** Default base package 
  *)
let default = 

  (*
     Base arguments
   *)

  let args =
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
      BaseArgExt.wth 
        "build_date"
        "date Date of build"
        (fun _ -> date_R ())
    in

    let args renv =
      List.fold_left
        (fun acc (name, hlp, dflt) ->
           renv := Env.var_define name (Env.var_expand dflt) !renv;
           (
             "--"^name,
             Arg.String (fun str -> renv := Env.var_set name str !renv),
             "dir "^hlp^" ["^(Env.var_get name !renv)^"]"
           ) :: acc
        )
        ((BaseArgExt.merge [arg_date; Env.args]) renv)
        lst
    in

      args
  in

  (*
     Base component check
   *)

  (** Check for standard program 
    *)
  let opt_prog prg = Chk.prog_best prg [prg^".opt"; prg]
  in

  let ocamlc     = opt_prog "ocamlc"
  in
  let ocamlopt   = opt_prog "ocamlopt"
  in
  let ocamlfind  = Chk.prog "ocamlfind"
  in

  (** Return ocaml version and check against a minimal version.
    *)
  let ocaml_min_version ?min_version env = 
    let ver =
      Env.var_get "ocaml_version" env
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
              env
        | None ->
            env
    in
      ver, nnenv
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
      (
        (Chk.fenv ocamlc)
        ::
        (BaseOCamlcConfig.init (Env.var_get "ocamlc"))
        ::
        (List.map 
           Chk.fenv
           [
             suffix_link;
             suffix_program;
             ocaml_min_version;
             ocamlc;
             ocamlbest;
             ocamlfind;
           ]
        )
      )
      env
  in

  (*
   
    Default base package
   
   *)

    {
      args     = args;
      checks   = checks;
      in_files = [];
    }
;;

