
(** Entry points for setup.ml
    @author Sylvain Le Gall
  *)

open BaseEnv
open OASISTypes

type std_args_fun = 
    package -> string array -> unit

type ('a, 'b) section_args_fun = 
    name * (package -> name -> 'a -> string array -> 'b)

type t =
    {
      configure:       std_args_fun;
      build:           std_args_fun;
      doc:             ((unit, unit)  section_args_fun) list;
      test:            ((test, float) section_args_fun) list;
      install:         std_args_fun;
      uninstall:       std_args_fun;
      clean:           std_args_fun list;
      clean_doc:       (unit, unit) section_args_fun list;
      clean_test:      (test, unit) section_args_fun list;
      distclean:       std_args_fun list;
      distclean_doc:   (unit, unit) section_args_fun list;
      distclean_test:  (test, unit) section_args_fun list;
      package:         package;
    } 

(** Configure step *)
let configure t args = 
  (* Run configure *)
  t.configure t.package args;

  (* Reload environment *)
  unload ();
  load ();

  (* Replace data in file *)
  BaseFileAB.replace t.package.files_ab

(** Build step *)
let build t args =
  t.build t.package args

(** Documentation step *)
let doc t args =
  List.iter
    (fun (nm, f) -> 
       let doc = 
         () 
       in
         f t.package nm doc args)
    t.doc

(** Test step *)
let test t args = 
  BaseTest.test 
    (List.map 
       (fun (nm, f) ->
          let test =
            List.assoc nm t.package.tests
          in
            f, nm, test)
       t.test)
    t.package
    args

(** Install step *)
let install t args =
  t.install t.package args

(** Uninstall step *)
let uninstall t args =
  t.uninstall t.package args

(** Clean and distclean steps *)
let clean, distclean = 
  let generic_clean t what mains docs tests args = 
    List.iter
      (fun (nm, f) ->
         let doc = 
           ()
         in
           f t.package nm doc args)
      docs;
    List.iter
      (fun (nm, f) ->
         let test = 
           try
             List.assoc nm t.package.tests
           with Not_found ->
             failwith
               (Printf.sprintf
                  "Cannot find test '%s' when %s"
                  nm what)
         in
           f t.package nm test args)
      tests;
    List.iter
      (fun f -> 
         f t.package args)
      mains
  in

  let clean t args =
    generic_clean t "cleaning" t.clean t.clean_doc t.clean_test args
  in

  let distclean t args =
    (* Call clean *)
    clean t args;

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
    generic_clean t "distcleaning" t.distclean t.distclean_doc t.distclean_test args
  in

    clean, distclean

let setup t = 
  try
    let act_ref =
      ref (fun _ -> 
             failwith
               (Printf.sprintf
                  "No action defined, run '%s %s -help'"
                  Sys.executable_name
                  Sys.argv.(0)))

    in
    let extra_args_ref =
      ref []
    in
    let allow_empty_env_ref = 
      ref false
    in

    let arg_handle ?(allow_empty_env=false) act =
      Arg.Tuple
        [
          Arg.Rest (fun str -> extra_args_ref := str :: !extra_args_ref);

          Arg.Unit 
            (fun () -> 
               allow_empty_env_ref := allow_empty_env;
               act_ref := act);
        ]
    in

      Arg.parse 
        [
          "-configure",
          arg_handle ~allow_empty_env:true configure,
          "[options*] Configure build process.";

          "-build",
          arg_handle build,
          "[options*] Run build process.";

          "-doc",
          arg_handle doc,
          "[options*] Build documentation.";

          "-test",
          arg_handle test,
          "[options*] Build and run tests.";

          "-install",
          arg_handle install,
          "[options*] Install library, data, executable and documentation.";

          "-uninstall",
          arg_handle uninstall,
          "[options*] Uninstall library, data, executable and documentation.";

          "-clean",
          arg_handle ~allow_empty_env:true clean,
          "[options*] Clean build environment.";

          "-distclean",
          arg_handle ~allow_empty_env:true distclean,
          "[options*] Clean build and configure environment.";
        ]
        (fun str -> failwith ("Don't know what to do with "^str))
        "Setup and run build process current package\n";

      (* Build initial environment *)
      load ~allow_empty:!allow_empty_env_ref ();

      (** Initialize flags *)
      List.iter 
        (fun (nm, {flag_description = hlp; flag_default = choices}) ->
           let apply ?short_desc () = 
             var_ignore
               (var_define
                  ~cli:CLIAuto
                  ?short_desc
                  nm
                  (lazy (string_of_bool (var_choose choices))))
           in
             match hlp with 
               | Some hlp ->
                   apply ~short_desc:hlp ()
               | None ->
                   apply ())
        t.package.flags;

      BaseStandardVar.init (t.package.name, t.package.version);

      !act_ref t (Array.of_list (List.rev !extra_args_ref))

  with e ->
    BaseMessage.error (Printexc.to_string e);

(* END EXPORT *)

module PLG = BasePlugin

let odn_of_oasis pkg = 

  let build_gen, pkg = 
    (PLG.plugin_build pkg.build_type) pkg
  in

  let test_odn, test_gens, pkg = 
    let test_odns, test_gens, pkg = 
      List.fold_left
        (fun (test_odns, test_gens, pkg) (nm, tst) ->
           let gen, pkg, tst = 
             (PLG.plugin_test tst.test_type) pkg nm tst
           in
             ODN.TPL [ODN.STR nm; PLG.odn_of_func gen.PLG.setup] :: test_odns,
             (nm, gen) :: test_gens,
             {pkg with tests = (nm, tst) :: pkg.tests})
        ([], [], {pkg with tests = []})
        pkg.tests
    in
      ODN.LST (List.rev test_odns),
      List.rev test_gens,
      {pkg with tests = List.rev pkg.tests}
  in

  let doc_odn, doc_gens, pkg = 
    ODN.LST [], [], pkg
  in

  let install_gen, pkg =
    (PLG.plugin_install pkg.install_type) pkg
  in

  let uninstall_gen, pkg =
    (PLG.plugin_uninstall pkg.install_type) pkg
  in

  let configure_gen, pkg =
    (PLG.plugin_configure pkg.conf_type) pkg
  in

  let std_gens = 
    [
      configure_gen;
      build_gen;
      install_gen;
      uninstall_gen;
    ]
  in

  let moduls = 
    let moduls_of_gen gen =
      gen.PLG.moduls
    in
    let moduls_of_gens lst =
      List.rev_map 
        moduls_of_gen 
        (List.rev_map snd lst)
    in
      List.flatten
        (List.flatten
           [
             List.map moduls_of_gen std_gens;
             moduls_of_gens doc_gens;
             moduls_of_gens test_gens;
           ])
  in

  let other_actions = 
    let of_gen gen =
      gen.PLG.other_action
    in
    let of_gens lst = 
      List.rev_map of_gen (List.rev_map snd lst)
    in
      List.flatten
        [
          List.map of_gen std_gens;
          of_gens doc_gens;
          of_gens test_gens;
        ]
  in

  let clean_funcs,     clean_doc_funcs,     clean_test_funcs,
      distclean_funcs, distclean_doc_funcs, distclean_test_funcs =
    let acc_non_opt f lst = 
      let acc_non_opt_aux acc e = 
        match f e with
          | Some v -> v :: acc
          | None -> acc
      in
        List.rev (List.fold_left acc_non_opt_aux [] lst)
    in

    let acc_non_opt_assoc f =
      let assoc_f (nm, e) =
        match f e with
          | Some v -> Some (nm, v)
          | None -> None
      in
        acc_non_opt assoc_f 
    in

    let clean_of_gen gen = gen.PLG.clean
    in
    let distclean_of_gen gen = gen.PLG.distclean
    in

      acc_non_opt clean_of_gen std_gens,
      acc_non_opt_assoc clean_of_gen doc_gens,
      acc_non_opt_assoc clean_of_gen test_gens,
      acc_non_opt distclean_of_gen std_gens,
      acc_non_opt_assoc distclean_of_gen doc_gens,
      acc_non_opt_assoc distclean_of_gen test_gens
  in

  let () = 
    (* Put some type constraints between plugin and
       BaseSetup.t
     *)
    if false then
      begin
        let setup_func_calls lst =
          List.map (fun (nm, gen) -> nm, PLG.func_call gen.PLG.setup) lst
        in
        let func_calls lst = 
          List.map (fun (nm, func) -> nm, PLG.func_call func) lst
        in
        let _t: t = 
          {
            configure       = PLG.func_call configure_gen.PLG.setup;
            build           = PLG.func_call build_gen.PLG.setup;
            doc             = setup_func_calls doc_gens;
            test            = setup_func_calls test_gens;
            install         = PLG.func_call install_gen.PLG.setup;
            uninstall       = PLG.func_call uninstall_gen.PLG.setup;
            clean           = List.map PLG.func_call clean_funcs;
            clean_test      = func_calls clean_test_funcs;
            clean_doc       = func_calls clean_doc_funcs;
            distclean       = List.map PLG.func_call distclean_funcs;
            distclean_test  = func_calls distclean_test_funcs;
            distclean_doc   = func_calls distclean_doc_funcs;
            package         = pkg;
          } 
        in
          ()
      end
  in

  let setup_t_odn =
    let odn_of_funcs lst =
      ODN.LST (List.map PLG.odn_of_func lst)
    in
    let odn_of_assocs lst =
      ODN.LST 
        (List.map 
           (fun (nm, func) -> 
              ODN.TPL[ODN.STR nm; PLG.odn_of_func func])
           lst)
    in
      ODN.REC
        ("BaseSetup",
         [
           "configure",      PLG.odn_of_func configure_gen.PLG.setup;
           "build",          PLG.odn_of_func build_gen.PLG.setup;
           "test",           test_odn;
           "doc",            doc_odn;
           "install",        PLG.odn_of_func  install_gen.PLG.setup;
           "uninstall",      PLG.odn_of_func  uninstall_gen.PLG.setup;
           "clean",          odn_of_funcs clean_funcs;
           "clean_test",     odn_of_assocs clean_test_funcs;
           "clean_doc",      odn_of_assocs clean_doc_funcs;
           "distclean",      odn_of_funcs distclean_funcs;
           "distclean_test", odn_of_assocs distclean_test_funcs;
           "distclean_doc",  odn_of_assocs distclean_doc_funcs;
           "package",        OASISTypes.odn_of_package pkg;
          ])
  in

    pkg, setup_t_odn, other_actions, moduls
