(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2008-2010, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

open BaseEnv
open BaseMessage
open OASISTypes
open OASISSection
open OASISGettext
open OASISUtils

type std_args_fun =
    package -> string array -> unit

type ('a, 'b) section_args_fun =
    name * (package -> (common_section * 'a) -> string array -> 'b)

type t =
    {
      configure:       std_args_fun;
      build:           std_args_fun;
      doc:             ((doc, unit)  section_args_fun) list;
      test:            ((test, float) section_args_fun) list;
      install:         std_args_fun;
      uninstall:       std_args_fun;
      clean:           std_args_fun list;
      clean_doc:       (doc, unit) section_args_fun list;
      clean_test:      (test, unit) section_args_fun list;
      distclean:       std_args_fun list;
      distclean_doc:   (doc, unit) section_args_fun list;
      distclean_test:  (test, unit) section_args_fun list;
      package:         package;
      version:         string;
    }

(* Associate a plugin function with data from package *)
let join_plugin_sections filter_map lst =
  List.rev
    (List.fold_left
       (fun acc sct ->
          match filter_map sct with
            | Some e ->
                e :: acc
            | None ->
                acc)
       []
       lst)

(* Search for plugin data associated with a section name *)
let lookup_plugin_section plugin action nm lst =
  try
    List.assoc nm lst
  with Not_found ->
    failwithf
      (f_ "Cannot find plugin %s matching section %s for %s action")
      plugin
      nm
      action

let configure t args =
  (* Run configure *)
  BaseCustom.hook
    t.package.conf_custom
    (t.configure t.package)
    args;

  (* Reload environment *)
  unload ();
  load ();

  (* Replace data in file *)
  BaseFileAB.replace t.package.files_ab

let build t args =
  BaseCustom.hook
    t.package.build_custom
    (t.build t.package)
    args

let doc t args =
  BaseDoc.doc
    (join_plugin_sections
       (function
          | Doc (cs, e) ->
              Some
                (lookup_plugin_section
                   "documentation"
                   (s_ "build")
                   cs.cs_name
                   t.doc,
                 cs,
                 e)
          | _ ->
              None)
       t.package.sections)
    t.package
    args

let test t args =
  BaseTest.test
    (join_plugin_sections
       (function
          | Test (cs, e) ->
              Some
                (lookup_plugin_section
                   "test"
                   (s_ "run")
                   cs.cs_name
                   t.test,
                 cs,
                 e)
          | _ ->
              None)
       t.package.sections)
    t.package
    args

let all t args =
  let rno_doc =
    ref false
  in
  let rno_test =
    ref false
  in
    Arg.parse_argv
      ~current:(ref 0)
      (Array.of_list
         ((Sys.executable_name^" all") ::
          (Array.to_list args)))
      [
        "-no-doc",
        Arg.Set rno_doc,
        s_ "Don't run doc target";

        "-no-test",
        Arg.Set rno_test,
        s_ "Don't run test target";
      ]
      (failwithf (f_ "Don't know what to do with '%s'"))
      "";

    info "Running configure step";
    configure t [||];

    info "Running build step";
    build     t [||];

    (* Load setup.log dynamic variables *)
    BaseDynVar.init t.package;

    if not !rno_doc then
      begin
        info "Running doc step";
        doc t [||];
      end
    else
      begin
        info "Skipping doc step"
      end;

    if not !rno_test then
      begin
        info "Running test step";
        test t [||]
      end
    else
      begin
        info "Skipping test step"
      end

let install t args =
  BaseCustom.hook
    t.package.install_custom
    (t.install t.package)
    args

let uninstall t args =
  BaseCustom.hook
    t.package.uninstall_custom
    (t.uninstall t.package)
    args

let reinstall t args =
  uninstall t args;
  install t args

let clean, distclean =
  let failsafe f a =
    try
      f a
    with e ->
      warning
        (f_ "Action fail with error: %s")
        (match e with
           | Failure msg -> msg
           | e -> Printexc.to_string e)
  in

  let generic_clean t cstm mains docs tests args =
    BaseCustom.hook
      ~failsafe:true
      cstm
      (fun () ->
         (* Clean section *)
         List.iter
           (function
              | Test (cs, test) ->
                  let f =
                    try
                      List.assoc cs.cs_name tests
                    with Not_found ->
                      fun _ _ _ -> ()
                  in
                    failsafe
                      (f t.package (cs, test))
                      args
              | Doc (cs, doc) ->
                  let f =
                    try
                      List.assoc cs.cs_name docs
                    with Not_found ->
                      fun _ _ _ -> ()
                  in
                    failsafe
                      (f t.package (cs, doc))
                      args
              | Library _
              | Executable _
              | Flag _
              | SrcRepo _ ->
                  ())
           t.package.sections;
         (* Clean whole package *)
         List.iter
           (fun f ->
              failsafe
                (f t.package)
                args)
           mains)
      ()
  in

  let clean t args =
    generic_clean
      t
      t.package.clean_custom
      t.clean
      t.clean_doc
      t.clean_test
      args
  in

  let distclean t args =
    (* Call clean *)
    clean t args;

    (* Remove generated file *)
    List.iter
      (fun fn ->
         if Sys.file_exists fn then
           begin
             info (f_ "Remove '%s'") fn;
             Sys.remove fn
           end)
      (BaseEnv.default_filename
       ::
       BaseLog.default_filename
       ::
       (List.rev_map BaseFileAB.to_filename t.package.files_ab));

    (* Call distclean code *)
    generic_clean
      t
      t.package.distclean_custom
      t.distclean
      t.distclean_doc
      t.distclean_test
      args
  in

    clean, distclean

let version t _ =
  print_endline t.version

let setup t =
  let catch_exn =
    ref true
  in
    try
      let act_ref =
        ref (fun _ ->
               failwithf
                 (f_ "No action defined, run '%s %s -help'")
                 Sys.executable_name
                 Sys.argv.(0))

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
          (Arg.align
             [
               "-configure",
               arg_handle ~allow_empty_env:true configure,
               s_ "[options*] Configure the whole build process.";

               "-build",
               arg_handle build,
               s_ "[options*] Build executables and libraries.";

               "-doc",
               arg_handle doc,
               s_ "[options*] Build documents.";

               "-test",
               arg_handle test,
               s_ "[options*] Run tests.";

               "-all",
               arg_handle ~allow_empty_env:true all,
               s_ "[options*] Run configure, build, doc and test targets.";

               "-install",
               arg_handle install,
               s_ "[options*] Install libraries, data, executables \
                              and documents.";

               "-uninstall",
               arg_handle uninstall,
               s_ "[options*] Uninstall libraries, data, executables \
                              and documents.";

               "-reinstall",
               arg_handle reinstall,
               s_ "[options*] Uninstall and install libraries, data, \
                              executables and documents.";

               "-clean",
               arg_handle ~allow_empty_env:true clean,
               s_ "[options*] Clean files generated by a build.";

               "-distclean",
               arg_handle ~allow_empty_env:true distclean,
               s_ "[options*] Clean files generated by a build and configure.";

               "-version",
               arg_handle ~allow_empty_env:true version,
               s_ " Display version of OASIS used to generate this setup.ml.";

               "-no-catch-exn",
               Arg.Clear catch_exn,
               s_ " Don't catch exception, useful for debugging.";
             ]
           @ (BaseContext.args ()))
          (failwithf (f_ "Don't know what to do with '%s'"))
          (s_ "Setup and run build process current package\n");

        (* Build initial environment *)
        load ~allow_empty:!allow_empty_env_ref ();

        (** Initialize flags *)
        List.iter
          (function
             | Flag (cs, {flag_description = hlp;
                          flag_default = choices}) ->
                 begin
                   let apply ?short_desc () =
                     var_ignore
                       (var_define
                          ~cli:CLIEnable
                          ?short_desc
                          (OASISUtils.varname_of_string cs.cs_name)
                          (lazy (string_of_bool
                                   (var_choose
                                      ~name:(Printf.sprintf
                                               (f_ "default value of flag %s")
                                               cs.cs_name)
                                      ~printer:string_of_bool
                                      choices))))
                   in
                     match hlp with
                       | Some hlp ->
                           apply ~short_desc:(fun () -> hlp) ()
                       | None ->
                           apply ()
                 end
             | _ ->
                 ())
          t.package.sections;

        BaseStandardVar.init t.package;

        BaseDynVar.init t.package;

        !act_ref t (Array.of_list (List.rev !extra_args_ref))

    with e when !catch_exn ->
      error "%s" (Printexc.to_string e);
      exit 1

(* END EXPORT *)

open OASISPlugin

let default_filename =
  "setup.ml"

let find ctxt =
  try
    OASISFileTemplate.find default_filename ctxt.files
  with Not_found ->
    failwithf
      (f_ "Cannot find setup template file '%s'")
      default_filename

let of_package pkg =

  let ctxt =
    (* Initial context *)
    {
      error         = false;
      files         = OASISFileTemplate.empty;
      other_actions = [];
      ctxt          = !BaseContext.default;
    }
  in

  let ctxt, configure_changes =
    (Configure.act pkg.conf_type) ctxt pkg
  in

  let ctxt, build_changes =
    (Build.act pkg.build_type) ctxt pkg
  in

  let ctxt, test_odn, test_changes =
    let ctxt, test_odns, test_changes =
      List.fold_left
        (fun ((ctxt, test_odns, test_changes) as acc) ->
           function
             | Test (cs, tst) ->
                 begin
                   let ctxt, chng =
                     (Test.act tst.test_type) ctxt pkg (cs, tst)
                   in
                     ctxt,
                     (ODN.TPL [ODN.STR cs.cs_name;
                               ODNFunc.odn_of_func chng.chng_main]
                      ::
                      test_odns),
                     (cs.cs_name, chng) :: test_changes
                 end
             | sct ->
                 acc)
        (ctxt, [], [])
        pkg.sections
    in
      ctxt,
      ODN.LST (List.rev test_odns),
      List.rev test_changes
  in

  let ctxt, doc_odn, doc_changes =
    let ctxt, doc_odns, doc_changes =
      List.fold_left
        (fun ((ctxt, doc_odns, doc_changes) as acc) ->
           function
             | Doc (cs, doc) ->
                 begin
                   let ctxt, chng =
                     (Doc.act doc.doc_type) ctxt pkg (cs, doc)
                   in
                     ctxt,
                     (ODN.TPL [ODN.STR cs.cs_name;
                               ODNFunc.odn_of_func chng.chng_main]
                      ::
                      doc_odns),
                     (cs.cs_name, chng) :: doc_changes
                 end
             | sct ->
                 acc)
        (ctxt, [], [])
        pkg.sections
    in
      ctxt,
      ODN.LST (List.rev doc_odns),
      List.rev doc_changes
  in

  let ctxt, install_changes, uninstall_changes =
    let inst, uninst =
      Install.act pkg.install_type
    in
    let ctxt, install_changes =
      inst ctxt pkg
    in
    let ctxt, uninstall_changes =
      uninst ctxt pkg
    in
      ctxt, install_changes, uninstall_changes
  in

  let ctxt =
    (* Run extra plugin *)
    List.fold_left
      (fun ctxt nm -> (Extra.act nm) ctxt pkg)
      ctxt
      pkg.plugins
  in

  let std_changes =
    [
      configure_changes;
      build_changes;
      install_changes;
      uninstall_changes;
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

    let clean_of_changes chng = chng.chng_clean
    in
    let distclean_of_changes chng = chng.chng_distclean
    in

      acc_non_opt clean_of_changes std_changes,
      acc_non_opt_assoc clean_of_changes doc_changes,
      acc_non_opt_assoc clean_of_changes test_changes,
      acc_non_opt distclean_of_changes std_changes,
      acc_non_opt_assoc distclean_of_changes doc_changes,
      acc_non_opt_assoc distclean_of_changes test_changes
  in

  let moduls =
    (* Extract and deduplicate modules *)
    let extract lst =
      List.map (fun chng -> chng.chng_moduls) lst
    in
    let moduls =
       List.flatten
         ([
           OASISData.oasissys_ml;
           BaseData.basesysenvironment_ml;
           BaseData.basesys_ml;
         ]
         ::
          ((extract std_changes) @
           (extract (List.map snd doc_changes)) @
           (extract (List.map snd test_changes))))
    in

    let rmoduls, _ =
      List.fold_left
        (fun ((moduls, moduls_seen) as acc) modul ->
           if SetString.mem modul moduls_seen then
             acc
           else
             (modul :: moduls, SetString.add modul moduls_seen))
        ([], SetString.empty)
        moduls
    in
      List.rev rmoduls
  in

  let ctxt =
    (* Create setup file *)
    let setup_t_odn =
      let odn_of_funcs lst =
        ODN.LST (List.map ODNFunc.odn_of_func lst)
      in
      let odn_of_assocs lst =
        ODN.LST
          (List.map
             (fun (nm, func) ->
                ODN.TPL[ODN.STR nm; ODNFunc.odn_of_func func])
             lst)
      in
        ODN.REC
          ("BaseSetup",
           [
             "configure",      ODNFunc.odn_of_func configure_changes.chng_main;
             "build",          ODNFunc.odn_of_func build_changes.chng_main;
             "test",           test_odn;
             "doc",            doc_odn;
             "install",        ODNFunc.odn_of_func  install_changes.chng_main;
             "uninstall",      ODNFunc.odn_of_func  uninstall_changes.chng_main;
             "clean",          odn_of_funcs clean_funcs;
             "clean_test",     odn_of_assocs clean_test_funcs;
             "clean_doc",      odn_of_assocs clean_doc_funcs;
             "distclean",      odn_of_funcs distclean_funcs;
             "distclean_test", odn_of_assocs distclean_test_funcs;
             "distclean_doc",  odn_of_assocs distclean_doc_funcs;
             "package",        OASISTypes.odn_of_package pkg;
             "version",        OASISVersion.odn_of_t OASISConf.version_full;
            ])
    in

    let setup_t_str =
      Format.fprintf Format.str_formatter
        "@[<hv2>let setup_t =@ %a;;@]"
        (ODN.pp_odn ~opened_modules:["OASISTypes"])
        setup_t_odn;
      Format.flush_str_formatter ()
    in

    let setup_tmpl =
      OASISFileTemplate.template_of_mlfile
        default_filename

         (* Header *)
         [
           "(* "^default_filename^" generated for the first time by "^
           "OASIS v"^(OASISVersion.string_of_version OASISConf.version_full)^" *)";
           "";
         ]
         (* Body *)
         (
          [
           "(*";
           "   Regenerated by OASIS v"^(OASISVersion.string_of_version OASISConf.version_full);
           "   Visit http://oasis.forge.ocamlcore.org for more information and";
           "   documentation about functions used in this file.";
           "*)";
          ]
          @
          moduls
          @
          [
            "open OASISTypes;;";
            "";
            setup_t_str;
            "";
            "let setup () = BaseSetup.setup setup_t;;";
            ""
          ])

       (* Footer *)
       ["let () = setup ();;"]
    in

      {ctxt with
           files = OASISFileTemplate.replace setup_tmpl ctxt.files}

  in

  let t =
    let setup_func_calls lst =
      List.map (fun (nm, chng) -> nm, ODNFunc.func_call chng.chng_main) lst
    in
    let func_calls lst =
      List.map (fun (nm, func) -> nm, ODNFunc.func_call func) lst
    in
      {
        configure       = ODNFunc.func_call configure_changes.chng_main;
        build           = ODNFunc.func_call build_changes.chng_main;
        doc             = setup_func_calls doc_changes;
        test            = setup_func_calls test_changes;
        install         = ODNFunc.func_call install_changes.chng_main;
        uninstall       = ODNFunc.func_call uninstall_changes.chng_main;
        clean           = List.map ODNFunc.func_call clean_funcs;
        clean_test      = func_calls clean_test_funcs;
        clean_doc       = func_calls clean_doc_funcs;
        distclean       = List.map ODNFunc.func_call distclean_funcs;
        distclean_test  = func_calls distclean_test_funcs;
        distclean_doc   = func_calls distclean_doc_funcs;
        package         = pkg;
        version         = OASISVersion.string_of_version OASISConf.version_full;
      }
  in

    ctxt, t
