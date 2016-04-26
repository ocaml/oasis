(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2013, Sylvain Le Gall                                   *)
(* Copyright (C) 2008-2011, OCamlCore SARL                                    *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)


(** Generate custom configure/build/doc/test/install system
    @author
*)


open BaseEnv
open OASISGettext
open OASISTypes


    TYPE_CONV_PATH "CustomPlugin"


type t =
  {
    cmd_main:      command_line conditional;
    cmd_clean:     (command_line option) conditional;
    cmd_distclean: (command_line option) conditional;
  } with odn


let run  = BaseCustom.run


let main t _ extra_args =
  let cmd, args =
    var_choose
      ~name:(s_ "main command")
      t.cmd_main
  in
  run cmd args extra_args


let clean t pkg extra_args =
  match var_choose t.cmd_clean with
    | Some (cmd, args) ->
      run cmd args extra_args
    | _ ->
      ()


let distclean t pkg extra_args =
  match var_choose t.cmd_distclean with
    | Some (cmd, args) ->
      run cmd args extra_args
    | _ ->
      ()


module Build =
struct
  let main t pkg extra_args =
    main t pkg extra_args;
    List.iter
      (fun sct ->
         let evs =
           match sct with
             | Library (cs, bs, lib) when var_choose bs.bs_build ->
               begin
                 let evs, _ =
                   BaseBuilt.of_library
                     OASISHostPath.of_unix
                     (cs, bs, lib)
                 in
                 evs
               end
             | Executable (cs, bs, exec) when var_choose bs.bs_build ->
               begin
                 let evs, _, _ =
                   BaseBuilt.of_executable
                     OASISHostPath.of_unix
                     (cs, bs, exec)
                 in
                 evs
               end
             | _ ->
               []
         in
         List.iter
           (fun (bt, bnm, lst) -> BaseBuilt.register bt bnm lst)
           evs)
      pkg.sections

  let clean t pkg extra_args =
    clean t pkg extra_args;
    (* TODO: this seems to be pretty generic (at least wrt to ocamlbuild
     * considering moving this to BaseSetup?
    *)
    List.iter
      (function
        | Library (cs, _, _) ->
          BaseBuilt.unregister BaseBuilt.BLib cs.cs_name
        | Executable (cs, _, _) ->
          BaseBuilt.unregister BaseBuilt.BExec cs.cs_name;
          BaseBuilt.unregister BaseBuilt.BExecLib cs.cs_name
        | _ ->
          ())
      pkg.sections

  let distclean t pkg extra_args =
    distclean t pkg extra_args
end


module Test =
struct
  let main t pkg (cs, test) extra_args =
    try
      main t pkg extra_args;
      0.0
    with Failure s ->
      BaseMessage.warning
        (f_ "Test '%s' fails: %s")
        cs.cs_name
        s;
      1.0

  let clean t pkg (cs, test) extra_args =
    clean t pkg extra_args

  let distclean t pkg (cs, test) extra_args =
    distclean t pkg extra_args
end


module Doc =
struct
  let main t pkg (cs, _) extra_args =
    main t pkg extra_args;
    BaseBuilt.register BaseBuilt.BDoc cs.cs_name []

  let clean t pkg (cs, _) extra_args =
    clean t pkg extra_args;
    BaseBuilt.unregister BaseBuilt.BDoc cs.cs_name

  let distclean t pkg (cs, _) extra_args =
    distclean t pkg extra_args
end


(* END EXPORT *)


module BuildRuntime = Build
module TestRuntime  = Test
module DocRuntime   = Doc


open OASISGettext
open ODN
open OASISTypes
open OASISValues
open OASISPlugin
open OASISSchema


let nm, ver =
  "Custom", Some OASISConf.version_short


let conf_plugin = `Configure, nm, ver
let conf_data   = data_new_property conf_plugin


let build_plugin = `Build, nm, ver
let build_data   = data_new_property build_plugin


let install_plugin = `Install, nm, ver
let install_data   = data_new_property install_plugin
let uninstall_data = data_new_property ~purpose:`Uninstall install_plugin


let test_plugin = `Test, nm, ver
let test_data   = data_new_property test_plugin


let doc_plugin = `Doc, nm, ver
let doc_data = data_new_property doc_plugin


(** Add standard fields
*)
let add_fields
    ~schema
    id
    data
    nm
    hlp
    hlp_clean
    hlp_distclean =
  let cmd_main =
    new_field_conditional
      schema
      id
      nm
      command_line
      (* TODO: remove when fun () -> s_ be replaced *)
      (fun () -> s_ hlp)
      data (fun _ t -> t.cmd_main)
  in
  let cmd_clean =
    new_field_conditional
      schema
      id
      (nm^"Clean")
      ~default:None
      (opt command_line)
      (* TODO: remove when fun () -> s_ be replaced *)
      (fun () -> s_ hlp_clean)
      data (fun _ t -> t.cmd_clean)
  in
  let cmd_distclean =
    new_field_conditional
      schema
      id
      (nm^"Distclean")
      ~default:None
      (opt command_line)
      (* TODO: remove when fun () -> s_ be replaced *)
      (fun () -> s_ hlp_distclean)
      data (fun _ t -> t.cmd_distclean)
  in
  let generator data =
    {
      cmd_main      = cmd_main data;
      cmd_clean     = cmd_clean data;
      cmd_distclean = cmd_distclean data;
    }
  in
  cmd_main, cmd_clean, cmd_distclean, generator


(** Standard custom handling
*)
let std id data nm hlp hlp_clean hlp_distclean =
  let cmd_main, cmd_clean, cmd_distclean, generator =
    add_fields ~schema:OASISPackage.schema
      id data nm hlp hlp_clean hlp_distclean
  in
  generator,
  fun ctxt pkg ->
    let t =
      generator pkg.schema_data
    in
    ctxt,
    {
      OASISPlugin.chng_moduls =
        [CustomData.customsys_ml];

      chng_main =
        ODNFunc.func_with_arg
          main ("CustomPlugin.main")
          t odn_of_t;

      chng_clean =
        Some
          (ODNFunc.func_with_arg
             clean ("CustomPlugin.clean")
             t odn_of_t);

      chng_distclean =
        Some
          (ODNFunc.func_with_arg
             distclean ("CustomPlugin.distclean")
             t odn_of_t);
    }


(* Configure plugin *)
let conf_init () =
  let self_id, id =
    Configure.create
      conf_plugin
  in
  let generator, doit =
    std
      id
      (* TODO: test if replacing conf_data -> build_data generates an error *)
      conf_data
      "Conf"
      (ns_ "Run command to configure.")
      (ns_ "Run command to clean configure step.")
      (ns_ "Run command to distclean configure step.")
  in
  Configure.register_act self_id doit;
  register_generator_package id conf_data generator


(* Build plugin *)
let build_init () =
  let self_id, id =
    Build.create build_plugin
  in
  let cmd_main, cmd_clean, cmd_distclean, generator =
    add_fields
      id
      build_data
      ~schema:OASISPackage.schema
      "Build"
      (ns_ "Run command to build.")
      (ns_ "Run command to clean build step.")
      (ns_ "Run command to distclean build step.")
  in
  let doit ctxt pkg =
    let t =
      generator pkg.schema_data
    in
    ctxt,
    {
      OASISPlugin.chng_moduls =
        [CustomData.customsys_ml];

      chng_main =
        ODNFunc.func_with_arg
          BuildRuntime.main ("CustomPlugin.Build.main")
          t odn_of_t;

      chng_clean =
        Some
          (ODNFunc.func_with_arg
             BuildRuntime.clean ("CustomPlugin.Build.clean")
             t odn_of_t);

      chng_distclean =
        Some
          (ODNFunc.func_with_arg
             BuildRuntime.distclean ("CustomPlugin.Build.distclean")
             t odn_of_t);
    }
  in
  Build.register_act self_id doit;
  register_generator_package id build_data generator


(* Install plugin *)
let install_init () =
  let self_id, id =
    Install.create install_plugin
  in
  let generate_install, doit_install =
    std
      id
      install_data
      "Install"
      (ns_ "Run command to install.")
      (ns_ "Run command to clean install step.")
      (ns_ "Run command to distclean install step.")
  in
  let generate_uninstall, doit_uninstall =
    std
      id
      uninstall_data
      "Uninstall"
      (ns_ "Run command to uninstall.")
      (ns_ "Run command to clean uninstall step.")
      (ns_ "Run command to distclean uninstall step.")
  in
  Install.register_act self_id (doit_install, doit_uninstall);
  register_generator_package id install_data generate_install;
  register_generator_package id uninstall_data generate_uninstall


(* Document plugin *)
let doc_init () =
  let self_id, id =
    Doc.create doc_plugin
  in
  let cmd_main, cmd_clean, cmd_distclean, generator =
    add_fields
      ~schema:OASISDocument.schema
      id
      build_data
      ""
      (ns_ "Run command to build documentation.")
      (ns_ "Run command to clean build documentation step.")
      (ns_ "Run command to distclean build documentation step.")
  in
  let doit ctxt pkg (cs, doc) =
    let t =
      generator cs.cs_data
    in
    ctxt,
    {
      OASISPlugin.chng_moduls =
        [CustomData.customsys_ml];

      chng_main =
        ODNFunc.func_with_arg
          DocRuntime.main ("CustomPlugin.Doc.main")
          t odn_of_t;

      chng_clean =
        Some
          (ODNFunc.func_with_arg
             DocRuntime.clean ("CustomPlugin.Doc.clean")
             t odn_of_t);

      chng_distclean =
        Some
          (ODNFunc.func_with_arg
             DocRuntime.distclean ("CustomPlugin.Doc.distclean")
             t odn_of_t);
    }
  in
  Doc.register_act self_id doit;
  register_generator_package id doc_data generator


(* Test plugin *)
let test_init () =
  let self_id, id =
    Test.create test_plugin
  in
  let test_clean =
    new_field_conditional
      OASISTest.schema
      id
      "Clean"
      ~default:None
      (opt command_line)
      (fun () ->
         s_ "Run command to clean test step.")
      test_data (fun _ t -> t.cmd_clean)
  in
  let test_distclean =
    new_field_conditional
      OASISTest.schema
      id
      "Distclean"
      ~default:None
      (opt command_line)
      (fun () ->
         s_ "Run command to distclean test step.")
      test_data (fun _ t -> t.cmd_distclean)
  in
  let generator data =
    {
      cmd_main      = [OASISExpr.EBool true, ("false", [])];
      cmd_clean     = test_clean data;
      cmd_distclean = test_distclean data;
    }
  in
  let doit ctxt pkg (cs, test) =
    let t =
      {(generator cs.cs_data) with cmd_main = test.test_command}
    in
    ctxt,
    {
      OASISPlugin.chng_moduls =
        [CustomData.customsys_ml];

      chng_main =
        ODNFunc.func_with_arg
          TestRuntime.main ("CustomPlugin.Test.main")
          t odn_of_t;

      chng_clean =
        Some
          (ODNFunc.func_with_arg
             TestRuntime.clean ("CustomPlugin.Test.clean")
             t odn_of_t);

      chng_distclean =
        Some
          (ODNFunc.func_with_arg
             TestRuntime.distclean ("CustomPlugin.Test.distclean")
             t odn_of_t);
    }
  in
  Test.register_act self_id doit;
  register_generator_package id test_data generator


let init () =
  register_help
    conf_plugin
    (help_default CustomData.readme_template_mkd);
  conf_init ();
  build_init ();
  install_init ();
  doc_init ();
  test_init ()

