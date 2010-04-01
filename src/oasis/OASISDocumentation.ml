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

(** Test schema and generator
    @author Sylvain Le Gall
  *)

(* END EXPORT *)

open OASISTypes
open OASISSchema
open OASISValues
open OASISUtils
open OASISGettext

let schema, generator =
  let schm =
   schema "Test"
  in
  let cmn_section_gen =
    OASISSection.section_fields (s_ "test") schm
  in
  let build_tools = 
    OASISBuildSection.build_tools_field schm
  in
  let typ =
    new_field schm "Type"
      ~default:(OASISPlugin.builtin "none") 
      OASISPlugin.Test.value
      (fun () ->
         s_ "Plugin to use to build documentation.")
  in
  let custom =
    OASISCustom.add_fields schm ""
      (fun () -> s_ "Command to run before building the doc.")
      (fun () -> s_ "Command to run after building the doc.")
  in
  let install_dir =
    new_field schm "InstallDir"
      ~default:"$docdir"
      (expand file)
      (fun () ->
         s_ "Default target directory to install data and documentation.")
  in
  let build, install, data_files = 
   OASISBuildSection.build_install_data_fields schm
  in
    schm,
    (fun nm data ->
       Doc
         (cmn_section_gen nm data,
          {
            doc_type        = typ data;
            doc_custom      = custom data;
            doc_build       = build data;
            doc_install     = install data;
            doc_install_dir = install_dir data;
            doc_data_files  = data_files data;
            doc_build_tools = build_tools data;
          }))
