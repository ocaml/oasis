let () = 
  Findlib.init ()
;;

open FileUtil;;
open FileUtil.StrUtil;;
open FilePath.DefaultPath;;

(** {1 Message to user}
  *)
module Message =
struct 
  (** Print a warning message 
    *)
  let warn str =
    prerr_endline str

  (** Print an error message and exit.
    *)
  let error str =
    prerr_endline str;
    exit 1

  (** Print information message.
    *)
  let info str = 
    print_endline str

  (** Print begin of line when checking for a feature.
    *)
  let checking str =
    print_string "checking for ";
    print_string str;
    print_string "... "

  (** Print end of line when checking for a feature.
    *)
  let result str =
    print_endline str

  (** Print result and return it.
    *)
  let result_wrap str =
    result str;
    str
end
;;

(** {1 Environment for configure variable}
  *)
module Environment =
struct 
  module Msg    = Message
  module MapVar = Map.Make(String)
  module SetFn  = Set.Make(String)

  (** Variable type
    *)
  type var =
      {
        value:     string;
        no_export: bool;
      }

  (** Environment type
    *)
  type env = 
      {
        vars:            var MapVar.t;
        fn:              string;
        temporary_files: SetFn.t;
      }

  (** Apply in turn function to modify environment
    *)
  let chain (lst: (env -> string * env) list) (env: env) =
    List.fold_left
      (fun env f -> snd (f env))
      env
      lst

  (** Add a variable to environment. [no_export] allow to store
    * a variable that will be hidden to user (not printed).
    *)
  let var_add ?(no_export=false) name vl env =
    let nvars =
      MapVar.add 
        name 
        {value = vl; no_export = no_export} 
        env.vars
    in
      {
        env with 
            vars  = nvars;
      }

  (** Retrieve a variable value from environment
    *)
  let var_get ?(mandatory=false) ?(error_extra_message="") name env =
    try 
      (MapVar.find name env.vars).value
    with Not_found ->
      (
        if mandatory then
          Msg.error ("variable '"^name^"' is not defined "^error_extra_message)
        else
          raise Not_found
      )

  (** Expand variable that can be found in string. [multi] control
    * wether the variable will be expanded until there is nothing to 
    * expand anymore. Variable follow definition of variable for 
    * {!Buffer.substitute}. To escape '$', you must use '$_'.
    *)
  let rec var_expand ?(multi=true) ?(error_extra_message="") str env =
    let all_vars =
      String.concat ", "
        (MapVar.fold 
           (fun k v acc -> if v.no_export then acc else k :: acc)
           env.vars 
           []
        )
    in
    let subst f str =
      let buffer_replaced =
        Buffer.create ((String.length str) * 2)
      in
        Buffer.add_substitute 
          buffer_replaced
          f
          str;
        Buffer.contents buffer_replaced
    in
    let nstr =
      subst 
        (fun varname ->
           if varname = "_" then
             "$_"
           else
             var_get 
               ~mandatory:true
               ~error_extra_message:("in '"^str^"' "^error_extra_message^
                                     "; available variables: "^all_vars)
               varname 
               env
        )
        str
    in
      if multi && nstr <> str then
        (
          var_expand 
            ~multi:multi 
            ~error_extra_message:error_extra_message 
            nstr
            env
        )
      else
        (
          (* Proceed with the final replacement of $_ by $ *)
          subst
            (function 
               | "_" ->
                   "$"
               | str ->
                   failwith 
                     ("Unknown replacement "^str^" "^error_extra_message)
            )
            nstr
        )

  (** Add a temporary file
    *)
  let temporary_add fn env = 
    {
      env with 
          temporary_files = SetFn.add fn env.temporary_files
    }

  (** Get temporary file list
    *)
  let temporary_get env =
    SetFn.elements env.temporary_files

  (** Try to get a variable value from environment and if it fails
    * compute it and store it in environment.
    *)
  let cache ?(no_export=false) name f env =
    try
      var_get name env, env
    with Not_found ->
      (
        let vl, nenv =
          f env
        in
        let nnenv =
          var_add ~no_export:no_export name vl nenv
        in
          vl, nnenv
      )

  (** Save environment on disk.
    *)
  let dump env = 
    let chn =
      open_out env.fn
    in
      Marshal.to_channel chn env [];
      close_out chn

  (** Initialize environment.
    *)
  let init ?filename pkg_name pkg_version = 
    let fn = 
      Filename.concat 
        (Filename.dirname Sys.argv.(0))
        (
          match filename with 
            | Some f ->
                f
            | None ->
                "build.data"
        )
    in
    let env =
      if Sys.file_exists fn then
        (
          let chn =
            open_in fn
          in
          let env =
            Marshal.from_channel chn
          in
            close_in chn;
            env
        )
      else
        {
          vars            = MapVar.empty;
          fn              = fn;
          temporary_files = SetFn.empty;
        }
    in
    List.fold_left
      (fun env f -> f env)
      env
      [
        var_add "pkg_name" pkg_name;
        var_add "pkg_version" pkg_version;
      ]

  (** Display environment to user.
    *)
  let print env =
    print_newline ();
    print_endline "Configuration: ";
    print_newline ();
    MapVar.iter 
      (fun name var ->
         if not var.no_export then
           print_endline (name^": "^(var_expand var.value env))
      )
      env.vars;
    print_newline ();
    print_endline "Temporary files: ";
    print_newline ();
    SetFn.iter print_endline env.temporary_files;
    print_newline ()

end
;;

(** {1 Handle command line argument}
  *)
module BuildArg =
struct
  open Environment

  let enable name hlp default renv =
    let () = 
      renv := var_add name (if default then "true" else "false") !renv
    in
      [
        "--enable-"^name,
        Arg.Unit (fun () -> renv := var_add name "true" !renv),
        " Enable "^hlp^(if default then " [default]" else "");

        "--disable-"^name,
        Arg.Unit (fun () -> renv := var_add name "false" !renv),
        " Disable "^hlp^(if not default then " [default]" else "");
      ]
   
  let wth name hlp default renv =
    let () = 
      renv := var_add name default !renv
    in
      [
        "--with-"^name,
        Arg.String (fun str -> renv := var_add name str !renv),
        hlp^" ["^default^"]"
      ]

  let base renv =
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
        "install architecture-dependent files in dir",
        "$prefix";

        "bindir",
        "user executables",
        Filename.concat "$eprefix" "bin";

        "sbindir",
        "system admin executables",
        Filename.concat "$eprefix" "sbin";

        "libexecdir",
        "program executables",
        Filename.concat "$eprefix" "libexec";

        "sysconfdir",
        "read-only single-machine data",
        Filename.concat "$prefix" "etc";

        "sharedstatedir",
        "modifiable architecture-independent data",
        Filename.concat "$prefix" "com";

        "localstatedir",
        "modifiable single-machine data",
        Filename.concat "$prefix" "var";

        "libdir",
        "object code libraries",
        Filename.concat "$eprefix" "lib";

        "datarootdir",
        "read-only arch.-independent data root",
        Filename.concat "$prefix" "share";

        "datadir",
        "read-only architecture-independent data",
        "$datarootdir";

        "infodir",
        "info documentation",
        Filename.concat "$datarootdir" "info";

        "localedir",
        "locale-dependent data",
        Filename.concat "$datarootdir" "locale";

        "mandir",
        "man documentation",
        Filename.concat "$datarootdir" "man";

        "docdir",
        "documentation root",
        Filename.concat (Filename.concat "$datarootdir" "doc") "$pkg_name";

        "htmldir",
        "html documentation",
        "$docdir";

        "dvidir",
        "dvi documentation",
        "$docdir";

        "pdfdir",
        "pdf documentation",
        "$docdir";

        "psdir",
        "ps documentation",
        "$docdir";
      ]
    in
      List.rev
        (
          List.fold_left
            (fun acc (name, hlp, dflt) ->
               let () = 
                 renv := var_add name dflt !renv
               in
                 ("--"^name,
                  Arg.String (fun str -> renv := var_add name str !renv),
                  hlp^" ["^dflt^"]"
                 ) :: acc
            )
            []
            lst
        )
end
;;

(** {1 Checking for particular features} 
  *)
module Check =
struct
  module Msg  = Message
  module Env  = Environment

  (** Look for a program among a list of alternative program
    * the first found is returned. 
    *)
  let prog_best prg prg_lst =
    Env.cache prg
      (fun env ->
         let () = 
           Msg.checking prg
         in
           try 
             let alternate = 
               List.find 
                 (fun prg -> try ignore(which prg); true with Not_found -> false)
                 prg_lst
             in
               Msg.result_wrap (which alternate), env
           with Not_found ->
             (
               Msg.result "Not found";
               raise Not_found
             )
      )

  (** Check the presence of a particular program.
    *)
  let prog ?if_not_found prg =
    prog_best 
      prg 
      [prg]

  (** Check version, following Sys.ocaml_version convention
    *)
  let version feature min_version version = 
    
    (* Extract version of a string *)
    let version_of_string ver = 
      (* Split a string at certain char *)
      let split_at c str conv default =
        try 
          let idx =
            String.index str c
          in
          let before =
            conv (String.sub str 0 idx)
          in
          let after =
            if idx + 1 < String.length str then
              String.sub str (idx + 1) ((String.length str) - idx - 1)
            else
              ""
          in
            before, after
        with Not_found ->
          (
            if str = "" then
              default, ""
            else
              conv str, ""
          )
      in

      (* Extract version component *)
      let (major, rst) = 
        split_at '.' ver int_of_string 0
      in

      let (minor, rst) = 
        split_at '.' rst int_of_string 0
      in

      let (patchlevel, additional_info) = 
        split_at '+' rst int_of_string 0
      in
        (major, minor, patchlevel, additional_info)
    in

    (* Really compare version provided *)
    let var = 
      feature^"_version_"^min_version
    in
      Env.cache ~no_export:true var
        (fun env ->
           let () = 
             Msg.checking (feature^" version (>= "^min_version^")");
           in
           let v1 =
             version_of_string min_version
           in
           let v2 =
             version_of_string 
               (
                 if version = "[Distributed with OCaml]" then
                   Sys.ocaml_version
                 else
                   version
               )
           in
             if v2 < v1 then
               (
                 Msg.result ("Version doesn't match ("^version^" < "^min_version^")");
                 raise Not_found
               )
             else
               Msg.result_wrap version, env
        )

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
            snd (version "ocaml version" ver Sys.ocaml_version nenv)
        | None ->
            nenv
    in
      ver, nnenv

  (** Check for standard program 
    *)
  let opt_prog prg = prog_best prg [prg^".opt"; prg]

  let ocamlc     = opt_prog "ocamlc"
  let ocamlopt   = opt_prog "ocamlopt"
  let ocamllex   = opt_prog "ocamllex"
  let ocamlyacc  = opt_prog "ocamlyacc"

  let ocamlbuild = prog "ocamlbuild"
  let ocamlfind  = prog "ocamlfind"
  let camlp4     = prog "camlp4"
  let camlidl    = prog "camlidl"
  let ocamlmklib = prog "ocamlmklib"
  let mkcamlp4   = prog "mkcamlp4"
  let ocamldoc   = prog "ocamldoc"
  let xsltproc   = prog "xsltproc"
  let xmllint    = prog "xmllint"

  (** Check for findlib package
    *)
  let package ?min_version pkg =
    Env.cache ("pkg_"^pkg)
      (fun env ->
         try 
           let default_msg =
             "findlib package "^pkg
           in
           let () = 
             Msg.checking default_msg
           in
           let dir =
             Findlib.package_directory pkg
           in
           let () = 
             Msg.result dir
           in
           let nenv = 
             match min_version with 
               | Some min_ver ->
                   let ver =
                     Findlib.package_property [] pkg "version"
                   in
                     snd (version default_msg min_ver ver env)
               | None -> 
                   env
           in
             dir, nenv
         with
           | Fl_package_base.No_such_package _ ->
               raise Not_found
      )

  (** Check what is the best target for platform (opt/byte)
    *)
  let ocamlbest =
    Env.cache "ocamlbest"
      (fun env ->
         try 
           let nenv = 
             snd (ocamlopt env)
           in
             Msg.checking "ocamlbest";
             (Msg.result_wrap "opt"), nenv
         with Not_found ->
           let nenv =
             snd (ocamlc env)
           in
             Msg.checking "ocamlbest";
             (Msg.result_wrap "byte"), nenv
      )
  
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

  (** Check for everything mandatory for building OCaml project
    *)
  let ocaml_base env = 
    "", 
    Env.chain
      [
        suffix_link;
        suffix_program;
        ocaml_version;
        ocamlc;
        ocamlbest;
      ]
      env

  (** Compute the default ocamlbuild flags (OS dependent)
    *)
  let ocamlbuild_flags =
    Env.cache "ocamlbuild_flags_full"
      (fun env ->
         Msg.checking "ocamlbuild flags";
         Msg.result_wrap 
           (match Sys.os_type with
              | "Win32" ->
                  "-classic-display -no-log -byte-plugin -install-lib-dir "^
                  (Filename.concat (Findlib.ocaml_stdlib ()) "ocamlbuild")
              | _ ->
                  ""
           ),
         env
      )
 
  (** Check for everything mandatory for building OCaml project with ocamlbuild
    *)
  let ocamlbuild_base env =
    let nenv =
      Env.chain 
        [
          ocaml_base;
          ocamlbuild;
          ocamlbuild_flags;
        ]
        env
    in
    let ocamlbest, nnenv = 
      ocamlbest nenv
    in
      "",
      match ocamlbest with 
        | "opt" ->
            Env.var_add "ocamlbuild_best_library" "cmxa"
              (Env.var_add "ocamlbuild_best_program" "native" nnenv)
        | "byte"  ->
            Env.var_add "ocamlbuild_best_library" "cma"
              (Env.var_add "ocamlbuild_best_program" "byte" nnenv)
        | str ->
            failwith 
              ("Don't know what to when ocamlbest is '"^str^"'")
end
;;

(** {1 Filename using environment}
  *)
module RelativeFilename =
struct
  module Env = Environment

  (** Rebase filename relative to build file
    *)
  let filename fn env =
    Filename.concat (Filename.dirname env.Env.fn) fn

  (** Rebase filename and add program suffix, if required
    *)
  let program fn env =
    (filename fn env)^(Env.var_get ~mandatory:true "suffix_program" env)

  (** Rebase filename and add link suffix, if required
    *)
  let link fn env =
    (filename fn env)^(Env.var_get ~mandatory:true "suffix_link" env)

  (* TODO: check for suffix *)
end
;;

(** {1 Act using value collected in environment}
  *)
module Action = 
struct 
  module Msg = Message
  module Env = Environment
  module RFilename = RelativeFilename

  (** [replace fn_in env] Replace all string of the form '$(var)' where var is
    * a variable that can be found in [env]. [fn_in] must finish with '.in'.
    * The target file is the source filename without the ending '.in'. The file
    * is updated only if replace all these strings give a different result than
    * the actual target file.
    * 
    * See {!Buffer.substitute} for a complete guide to variable.
    *)
  let replace fn_in env =
    (** Make a filename relative to the
      * build file.
      *)
    let () =
      if not (Filename.check_suffix fn_in ".in") then
        failwith ("File "^fn_in^" doesn't end with '.in'")
    in
    let fn_in_rebase =
      RFilename.filename fn_in env 
    in
    let fn_rebase =
      Filename.chop_suffix fn_in_rebase ".in"
    in
    let chn_in =
      open_in fn_in_rebase
    in
    let equal_cur, close_cur, size_cur =
      if Sys.file_exists fn_rebase then
        (
          let chn =
            open_in fn_rebase
          in
            (fun str -> 
               try 
                 let line = 
                   input_line chn 
                 in 
                   line = str 
               with End_of_file ->
                 false
            ),
            (fun () -> close_in chn),
            in_channel_length chn
        )
      else
        (fun _ -> false),
        (fun () -> ()),
        (-1)
    in
    let buffer =
      Buffer.create (2 * (in_channel_length chn_in))
    in
    let rec read_check_replace line_num need_rewrite =
      try
        let line =
          input_line chn_in
        in
        let line_replaced =
          Env.var_expand 
            ~error_extra_message:("at line "^(string_of_int line_num)^
                                  " file "^(fn_in_rebase))
            line
            env
        in
        let () = 
          Buffer.add_string buffer line_replaced;
          Buffer.add_char   buffer '\n'
        in
        let line_updated = 
          (not (equal_cur line_replaced))
        in
        let nneed_rewrite =
          need_rewrite || line_updated
        in
          read_check_replace (line_num + 1) nneed_rewrite
      with End_of_file ->
        (
          close_cur ();
          if need_rewrite || (Buffer.length buffer) <> size_cur then
            (
              Msg.info ("Writing file "^fn_rebase);
              let chn_out =
                open_out fn_rebase
              in
                Buffer.output_buffer chn_out buffer
            )
        )
    in
      read_check_replace 1 false;
      Env.temporary_add fn_rebase env

  (** Parse command line arguments 
    *)
  let parse lst env =
    let renv =
      ref env
    in
    let rtargets = 
      ref []
    in
    let args =
      List.flatten 
        (List.map (fun f -> f renv) lst)
    in
      Arg.parse 
        (Arg.align args)
        (fun str -> rtargets := str :: !rtargets)
        (Env.var_expand 
           "\
           Build system for $pkg_name v$pkg_version\n\
           \n\
           Options: \n\n\
           " 
           env);
      (List.rev !rtargets), !renv


  (** Build environment using provided series of check to be done
    * and then output corresponding file.
    *)
  let configure pkg_name pkg_version lst_args lst_check lst_in =
    let env =
        (Env.init pkg_name pkg_version)
    in
    let targets, nenv =
      parse lst_args env
    in
    let nenv =
      Env.chain lst_check nenv
    in
    let nenv =
      List.fold_left 
        (fun env fn_in -> replace fn_in env) 
        nenv
        lst_in
    in
      if env <> nenv then
        (
          Env.print nenv;
          Env.dump  nenv
        );
      targets, nenv
 
  (** Execute a command
    *)
  let exec ?(exit_on_error=true) lst env = 
    let cmd = 
      String.concat " " lst 
    in
      Msg.info ("+ "^cmd);
      match Sys.command cmd with
        | 0 ->
            ()
        | i ->
            Msg.warn ("'"^cmd^"' exit with status "^(string_of_int i));
            (if exit_on_error then 
              exit i
            else
              ()
            )

  let clean_targets clean_fun clean_fns distclean_fun distclean_fns =
    [
      "clean",
      (fun env ->
         clean_fun env;
         rm ~recurse:true clean_fns
      );

      "distclean",
      (fun env ->
         clean_fun env;
         distclean_fun env;
         rm ~recurse:true (env.Env.fn :: (clean_fns @ distclean_fns @ (Env.temporary_get env)))
      )
    ]

  let ocamlbuild targets env =
    exec 
      ("ocamlbuild" :: 
       (Env.var_get ~mandatory:true "ocamlbuild_flags_full" env) :: 
       targets) 
      env

  let ocamlbuild_targets targets =
    [
      "all",
      (ocamlbuild targets);
    ]
    @
    (clean_targets (ocamlbuild ["-clean"]) [] ignore [])
    @
    (List.map
       (fun tgt -> tgt, (ocamlbuild [tgt]))
       targets
    )

  let process_targets ?default_tgt f_targets targets env =
    let process_one_target tgt =
      try
        let action = 
          List.assoc tgt f_targets
        in
          action env
      with Not_found ->
        failwith ("Unknown target "^tgt)
    in
      match targets with 
        | [] ->
            (match default_tgt, f_targets with 
               | Some tgt, _ 
               | None, (tgt, _) :: _ ->
                   Msg.info ("Using default target "^tgt);
                   process_one_target tgt
               | None, [] ->
                   ()
            )
        | lst ->
            List.iter process_one_target lst

end
;;
