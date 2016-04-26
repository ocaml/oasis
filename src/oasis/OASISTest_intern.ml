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


(** Test schema and generator
    @author Sylvain Le Gall
*)


(* END EXPORT *)


open OASISTypes
open OASISSchema_intern
open OASISValues
open OASISUtils
open OASISGettext


let schema, generator =
  let schm =
    schema "Test" (fun (cs, _) -> cs.cs_plugin_data)
  in
  let cmn_section_gen =
    OASISSection_intern.section_fields
      (fun () -> (s_ "test"))
      schm
      (fun (cs, _) -> cs)
  in
  let typ =
    new_field_plugin schm "Type"
      ~default:(OASISPlugin.builtin `Test "custom")
      `Test
      OASISPlugin.Test.value
      (fun () ->
         s_ "Plugin to use to run test.")
      (fun (_, test) -> test.test_type)
  in
  let tools =
    new_field schm "TestTools"
      ~default:[]
      OASISBuildSection_intern.build_tools_value
      (fun () ->
         s_ "Tools required to run the test, including internal executables.")
      (fun (_, test) -> test.test_tools)
  in
  let command =
    new_field_conditional schm "Command"
      command_line
      (fun () ->
         s_ "Command to run for the test.")
      (fun (_, test) -> test.test_command)
  in
  let working_directory =
    new_field schm "WorkingDirectory"
      ~default:None
      (opt string_not_empty)
      (fun () ->
         s_ "Directory to run the test.")
      (fun (_, test) -> test.test_working_directory)
  in
  let custom =
    OASISCustom.add_fields schm ""
      (fun () -> s_ "Command to run before the test")
      (fun () -> s_ "Command to run after the test")
      (fun (_, test) -> test.test_custom)
  in
  let run =
    new_field_conditional schm "Run"
      ~default:true
      boolean
      (fun () ->
         s_ "Enable this test.")
      (fun (_, test) -> test.test_run)
  in
  schm,
  (fun features_data nm data ->
     let cs =
       cmn_section_gen features_data nm data
     in
     (* Set data specific to plugin used for this test *)
     let typ =
       typ data
     in
     let rplugin_data =
       ref cs.cs_plugin_data
     in
     let cs =
       OASISPlugin.generator_section
         `Test
         (typ :> plugin_kind plugin)
         rplugin_data
         cs.cs_data;
       {cs with cs_plugin_data = !rplugin_data}
     in
     let run =
       if OASISFeatures.data_test OASISFeatures.flag_tests features_data then
         (* TODO: establish a formal link between here and BaseStandardVars *)
         OASISExpr.if_then_else
           (OASISExpr.EFlag "tests") (run data) [OASISExpr.EBool true, false]
       else
         run data
     in
     Test
       (cs,
        {
          test_type              = typ;
          test_command           = command data;
          test_working_directory = working_directory data;
          test_custom            = custom data;
          test_run               = run;
          test_tools             = tools data;
        }))
