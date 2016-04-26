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


let run_command  = BaseCustom.run


module BuildRuntime =
struct
  let main run pkg extra_args =
    run_command "omake" ( ["build"] @ OMakeFields.(run.extra_args)) extra_args;
    (* Register generated files in the log files. *)
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

  let clean run pkg extra_args =
    run_command "omake" (["clean"] @ OMakeFields.(run.extra_args)) extra_args;
    List.iter
      (function
        | Library (cs, _, _) ->
          BaseBuilt.unregister BaseBuilt.BLib cs.cs_name
        | Executable (cs, _, _) ->
          BaseBuilt.unregister BaseBuilt.BExec cs.cs_name;
          BaseBuilt.unregister BaseBuilt.BExecLib cs.cs_name
        | Doc(cs, _) ->
          (* "omake clean" also cleans docs *)
          BaseBuilt.unregister BaseBuilt.BDoc cs.cs_name
        | _ ->
          ())
      pkg.sections

  let distclean run pkg extra_args =
    run_command "omake" (["distclean"] @ OMakeFields.(run.extra_args))
      extra_args;
    List.iter
      (fun fn -> if Sys.file_exists fn then Sys.remove fn)
      [".omakedb"; ".omakedb.lock"]

end


module InstallRuntime = struct
  let install run pkg extra_args =
    run_command "omake" (["install"] @ OMakeFields.(run.extra_args)) extra_args

  let uninstall run pkg extra_args =
    run_command "omake" (["uninstall"] @ OMakeFields.(run.extra_args))
      extra_args
end


module DocRuntime = struct
  open OMakeFields
  let main run pkg (cs,doc) extra_args =
    let target =
      OASISUnixPath.make
        [ run.run_path;
          cs.cs_name ^ ".doc";
          string_of_format doc.doc_format;
        ] in
    run_command "omake" ([target] @ OMakeFields.(run.extra_args)) extra_args;
    (* The following exists only to make the internal install plugin happy: *)
    List.iter
      (fun sct ->
         match sct with
           | Doc(cs,doc) when var_choose doc.doc_build ->
             let files =
               Array.to_list
                 (Sys.readdir (OASISHostPath.of_unix target)) in
             let files =
               List.filter
                 (fun n -> n.[0] <> '.')
                 files in
             let full_files =
               List.map
                 (fun n ->
                    OASISHostPath.of_unix (OASISUnixPath.concat target n)
                 )
                 files in
             BaseBuilt.register
               BaseBuilt.BDoc
               cs.cs_name
               [full_files]
           | _ ->
             ()
      )
      pkg.sections
end


(* END EXPORT *)


open OASISGettext
open ODN
open OASISTypes
open OASISValues
open OASISPlugin
open OASISSchema
open OMakeFields


(* Build plugin *)
let build_init () =
  let generator data =
    { OMakeFields.run_path = "";
      OMakeFields.extra_args = OMakeFields.BuildFields.extra_args data;
    } in
  let doit ctxt pkg =
    let run =
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
          run OMakeFields.odn_of_run_t;

      chng_clean =
        Some
          (ODNFunc.func_with_arg
             BuildRuntime.clean ("OMakePlugin.BuildRuntime.clean")
             run OMakeFields.odn_of_run_t);

      chng_distclean =
        Some
          (ODNFunc.func_with_arg
             BuildRuntime.distclean ("OMakePlugin.BuildRuntime.distclean")
             run OMakeFields.odn_of_run_t);
    }
  in
  Build.register_act BuildFields.self_id doit;
  register_generator_package BuildFields.id BuildFields.build_data generator


(* Install plugin *)
let install_init () =
  let generator_inst data =
    { OMakeFields.run_path = "";
      OMakeFields.extra_args = []
    } in
  let generator_uninst data =
    { OMakeFields.run_path = "";
      OMakeFields.extra_args = []
    } in
  let doit_install ctxt pkg =
    let run =
      generator_inst pkg.schema_data in
    let equip() =
      OMakeEquip.equip_project ctxt pkg in
    { ctxt with other_actions = equip :: ctxt.other_actions },
    { chng_moduls = [OMakeData.omakesys_ml];
      chng_main = ODNFunc.func_with_arg InstallRuntime.install "OMakePlugin.InstallRuntime.install" run OMakeFields.odn_of_run_t;
      chng_clean = None;
      chng_distclean = None
    } in
  let doit_uninstall ctxt pkg =
    let run =
      generator_uninst pkg.schema_data in
    let equip() =
      OMakeEquip.equip_project ctxt pkg in
    { ctxt with other_actions = equip :: ctxt.other_actions },
    { chng_moduls = [OMakeData.omakesys_ml];
      chng_main = ODNFunc.func_with_arg InstallRuntime.uninstall "OMakePlugin.InstallRuntime.uninstall" run OMakeFields.odn_of_run_t;
      chng_clean = None;
      chng_distclean = None
    } in
  Install.register_act InstallFields.self_id (doit_install, doit_uninstall);
  register_generator_package InstallFields.id InstallFields.install_data generator_inst;
  register_generator_package InstallFields.id InstallFields.uninstall_data generator_uninst


(* Doc plugin *)
let doc_init () =
  let generator data =
    (* bug in OASIS? DocFields.path gives here always PropList.Not_set exn *)
    { OMakeFields.run_path = DocFields.path data;
      OMakeFields.extra_args = BuildFields.extra_args data;
    } in
  let doit ctxt pkg (cs,doc) =
    let run =
      generator cs.cs_data in
    let equip() =
      OMakeEquip.equip_project ctxt pkg in
    { ctxt with other_actions = equip :: ctxt.other_actions },
    {
      OASISPlugin.chng_moduls =
        [OMakeData.omakesys_ml];

      chng_main =
        ODNFunc.func_with_arg
          DocRuntime.main ("OMakePlugin.DocRuntime.main")
          run OMakeFields.odn_of_run_t;

      chng_clean = None;
      chng_distclean = None;
    }
  in
  Doc.register_act DocFields.self_id doit;
  register_generator_section `Doc DocFields.id DocFields.doc_data generator


let init () =
  register_help
    BuildFields.build_plugin
    (help_default OMakeData.readme_template_mkd);
  build_init ();
  install_init ();
  doc_init()

