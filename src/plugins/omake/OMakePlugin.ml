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


(** Generate omake configure/build/doc/test/install system
    @author Gerd Stolpmann
  *)


open BaseEnv
open OASISGettext
open OASISTypes


let run  = BaseCustom.run

module BuildRuntime =
struct
  let main t pkg extra_args =
    run "omake" ["build"] extra_args;
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
    run "omake" ["clean"] extra_args;
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
    run "omake" ["distclean"] extra_args
end


module InstallRuntime = struct
  let install t pkg extra_args =
    run "omake" ["install"] extra_args

  let uninstall t pkg extra_args =
    run "omake" ["uninstall"] extra_args
end


(*
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
 *)

(*
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
 *)

(* END EXPORT *)


(*
module TestRuntime  = Test
module DocRuntime   = Doc
 *)

open OASISGettext
open ODN
open OASISTypes
open OASISValues
open OASISPlugin
open OASISSchema
open OMakeFields


(* Build plugin *)
let build_init () =
  let generator data = { OMakeFields.extra_args = [] } in
  let doit ctxt pkg =
    let t =
      generator pkg.schema_data in
    let equip() =
      OMakeEquip.equip_project ctxt pkg in
    { ctxt with other_actions = equip :: ctxt.other_actions },
    {
      OASISPlugin.chng_moduls =
        [OMakeData.omakesys_ml];
      
      chng_main =
        ODNFunc.func_with_arg
          BuildRuntime.main ("OMakePlugin.BuildRuntime.main")
          t OMakeFields.odn_of_run_t;
      
      chng_clean =
        Some
          (ODNFunc.func_with_arg
             BuildRuntime.clean ("OMakePlugin.BuildRuntime.clean")
             t OMakeFields.odn_of_run_t);
      
      chng_distclean =
        Some
          (ODNFunc.func_with_arg
             BuildRuntime.distclean ("OMakePlugin.BuildRuntime.distclean")
             t OMakeFields.odn_of_run_t);
    }
  in
  Build.register_act BuildFields.self_id doit;
  register_generator_package BuildFields.id BuildFields.build_data generator


(* Install plugin *)
let install_init () =
  let generator_inst data = { OMakeFields.extra_args = [] } in
  let generator_uninst data = { OMakeFields.extra_args = [] } in
  let doit_install ctxt pkg =
    let t =
      generator_inst pkg.schema_data in
    let equip() =
      OMakeEquip.equip_project ctxt pkg in
    { ctxt with other_actions = equip :: ctxt.other_actions },
    { chng_moduls = [OMakeData.omakesys_ml];
      chng_main = ODNFunc.func_with_arg InstallRuntime.install "OMakePlugin.InstallRuntime.install" t OMakeFields.odn_of_run_t;
      chng_clean = None;
      chng_distclean = None
    } in
  let doit_uninstall ctxt pkg =
    let t =
      generator_uninst pkg.schema_data in
    let equip() =
      OMakeEquip.equip_project ctxt pkg in
    { ctxt with other_actions = equip :: ctxt.other_actions },
    { chng_moduls = [OMakeData.omakesys_ml];
      chng_main = ODNFunc.func_with_arg InstallRuntime.uninstall "OMakePlugin.InstallRuntime.uninstall" t OMakeFields.odn_of_run_t;
      chng_clean = None;
      chng_distclean = None
    } in
  Install.register_act InstallFields.self_id (doit_install, doit_uninstall);
  register_generator_package InstallFields.id InstallFields.install_data generator_inst;
  register_generator_package InstallFields.id InstallFields.uninstall_data generator_uninst


let init () =
  register_help
    BuildFields.build_plugin
    (help_default OMakeData.readme_template_mkd);
  build_init ();
  install_init ()
