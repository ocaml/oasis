
(** Entry point for ocaml-autobuild
    @author Sylvain Le Gall
  *)

open BaseEnv
open OASISTypes

type action_fun = string array -> unit;;

type t =
    {
      configure:       action_fun;
      build:           action_fun;
      doc:             action_fun list;
      test:            BaseTest.t list;
      install:         action_fun;
      uninstall:       action_fun;
      clean:           (unit -> unit) list;
      distclean:       (unit -> unit) list;
      package:         package;
    } 

let clean t = 
  List.iter
    (fun f -> f ())
    t.clean

let distclean t =
  (* Call clean *)
  clean t;

  (* Remove generated file *)
  List.iter
    (fun fn ->
       if Sys.file_exists fn then
         (BaseMessage.info 
            (Printf.sprintf "Remove '%s'" fn);
          Sys.remove fn))
    (BaseEnv.default_filename 
     :: 
     BaseLog.default_filename
     ::
     (List.rev_map BaseFileAB.to_filename t.package.files_ab));
  
  (* Call distclean code *)
  List.iter
    (fun f -> f ())
    t.distclean

let configure t args = 
  (* Run configure *)
  t.configure args;

  (* Replace data in file *)
  BaseFileAB.replace t.package.files_ab

let setup t = 
  try
    let act =
      ref (fun _ -> 
             failwith
               (Printf.sprintf
                  "No action defined, run '%s %s -help'"
                  Sys.executable_name
                  Sys.argv.(0)))

    in
    let extra_args =
      ref []
    in
    let allow_empty_env = 
      ref false
    in
    let arg_rest ?(configure=false) lst =
      Arg.Tuple
        [
          Arg.Rest (fun str -> extra_args := str :: !extra_args);
          Arg.Unit 
            (fun () ->
               allow_empty_env := configure;
               act :=
               (let args =
                  !extra_args 
                in
                  fun () ->
                    List.iter
                      (fun f -> 
                         f (Array.of_list (List.rev args)))
                      lst);
               extra_args := []); 
        ]
    in
    let arg_clean a =
      Arg.Unit 
        (fun () -> 
           allow_empty_env := true; 
           act := (fun () -> a ()));
    in

      Arg.parse 
        [
          "-configure",
          arg_rest ~configure:true [configure t],
          "[options*] Configure build process.";

          "-build",
          arg_rest [t.build],
          "[options*] Run build process.";

          "-doc",
          arg_rest t.doc,
          "[options*] Build documentation.";

          "-test",
          arg_rest [BaseTest.test t.test],
          "[options*] Build and run tests.";

          "-install",
          arg_rest [t.install],
          "[options*] Install library, data, executable and documentation.";

          "-uninstall",
          arg_rest [t.uninstall],
          "[options*] Uninstall library, data, executable and documentation.";

          "-clean",
          arg_clean (fun () -> clean t),
          "[options*] Clean build environment.";

          "-distclean",
          arg_clean (fun () -> distclean t),
          "[options*] Clean build and configure environment.";
        ]
        (fun str -> failwith ("Don't know what to do with "^str))
        "Setup and run build process current package\n";

      (* Build initial environment *)
      load ~allow_empty:!allow_empty_env ();

      (** Initialize flags *)
      List.iter 
        (fun (nm, {flag_description = hlp; flag_default = choices}) ->
           let apply ?short_desc () = 
             var_ignore
               (var_define
                  ~cli:CLIAuto
                  ?short_desc
                  nm
                  (lazy 
                     (string_of_bool
                       (OASISExpr.choose 
                          var_get 
                          (function
                             | TOs_type       -> var_get "os_type"
                             | TSystem        -> var_get "system"
                             | TArchitecture  -> var_get "architecture"
                             | TCcomp_type    -> var_get "ccomp_type"
                             | TOCaml_version -> var_get "ocaml_version")
                          choices))))
           in
             match hlp with 
               | Some hlp ->
                   apply ~short_desc:hlp ()
               | None ->
                   apply ())
        t.package.flags;

      BaseStandardVar.init (t.package.name, t.package.version);

      !act ()

  with e ->
    BaseMessage.error (Printexc.to_string e);

(* END EXPORT *)

open OASISTypes;;
open BasePlugin;;
open ODN;;

let code_of_oasis pkg = 

  let build_gen, pkg = 
    (plugin_build pkg.build_type) pkg
  in

  let test_gens, pkg = 
    let test_gens = 
      List.fold_left
        (fun test_gens (nm, tst) ->
           let gen, pkg = 
             (plugin_test tst.test_type) tst
           in
             ((nm, tst, gen) :: test_gens))
        []
        pkg.tests
    in
      List.rev test_gens,
      {pkg with tests = List.rev_map (fun (nm, tst, _) -> nm, tst) test_gens}
  in

  let install_gen, pkg =
    (plugin_install pkg.install_type) pkg
  in

  let uninstall_gen, pkg =
    (plugin_uninstall pkg.install_type) pkg
  in

  let configure_gen =
    (* We call last configure, so that we can collect variables and changes to
     * package defined in other plugins
     *)
    (plugin_configure pkg.conf_type) pkg
  in

  let all_actions =
    List.flatten 
      [[
         configure_gen;
         build_gen;
       ];
       (List.map 
          (fun (_, _, gen) -> gen)
          test_gens);
       [
         install_gen;
         uninstall_gen;
       ]]
  in

  let clean_code =
    (* Process clean code in reverse order *)
    LST
      (List.fold_left 
         (fun acc ->
            function
              | {clean_code = Some f} -> f :: acc
              | _ -> acc)
         []
         all_actions)
  in

  let distclean_code =
    (* Process distclean code in reverse order *)
    LST
      (List.fold_left
        (fun acc ->
          function
            | {distclean_code = Some f} -> f :: acc
            | _ -> acc)
        []
        all_actions)
  in

  let doc_code = 
    ODN.of_list (fun i -> i) []
  in

  let test_code =
    BaseTest.generate test_gens
  in

  let package_code =
    OASISTypes.odn_of_package pkg
  in

  let setup_t_code =
    REC
      ("BaseSetup",
       [
         "configure",  configure_gen.setup_code;
         "build",      build_gen.setup_code;
         "test",       test_code;
         "doc",        doc_code;
         "install",    install_gen.setup_code;
         "uninstall",  uninstall_gen.setup_code;
         "clean",      clean_code;
         "distclean",  distclean_code;
         "package",    package_code;
        ])
  in

    pkg, setup_t_code, all_actions
;;
