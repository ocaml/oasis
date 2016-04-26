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


(** Executable schema and generator
    @author Sylvain Le Gall
*)


open OASISTypes
open OASISSchema_intern
open OASISValues
open OASISUtils
open OASISGettext
open PropList.Field


let schema, generator =
  let schm =
    schema "Executable" (fun (cs, _, _) -> cs.cs_plugin_data)
  in
  let cmn_section_gen =
    OASISSection_intern.section_fields
      (fun () -> (s_ "executable")) schm
      (fun (cs, _, _) -> cs)
  in
  let build_section_gen =
    OASISBuildSection_intern.section_fields
      (fun () -> (s_ "executable")) Byte schm
      (fun (_, bs, _) -> bs)
  in
  let main_is =
    new_field schm "MainIs"
      (let base_value =
         {
           parse =
             (fun ~ctxt str ->
                if not (Filename.check_suffix str ".ml") then
                  failwithf
                    (f_ "'%s' is not  a '.ml' file")
                    str;
                str);
           update = update_fail;
           print  = (fun s -> s);
         }
       in
       {
         parse  = (fun ~ctxt str ->
           file.parse ~ctxt
             (base_value.parse ~ctxt str));
         update = update_fail;
         print  = (fun fn -> file.print (base_value.print fn));
       })
      (fun () ->
         s_ "OCaml file (.ml) containing main procedure for the executable.")
      (fun (_, _, exec) -> exec.exec_main_is)
  in
  let custom =
    new_field schm "Custom"
      ~default:false
      boolean
      (fun () ->
         s_ "Create custom bytecode executable.")
      (fun (_, _, exec) -> exec.exec_custom)
  in
  schm,
  (fun features_data nm data ->
     Executable
       (cmn_section_gen features_data nm data,
        build_section_gen nm data,
        {
          exec_main_is = main_is data;
          exec_custom  = custom data;
        }))
