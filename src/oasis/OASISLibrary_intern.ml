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


(** Library schema and generator
    @author Sylvain Le Gall
*)


open OASISTypes
open OASISSchema_intern
open OASISValues
open OASISGettext
open PropList.Field


let schema, generator =
  let schm =
    schema "Library" (fun (cs, _, _) -> cs.cs_plugin_data)
  in
  let cmn_section_gen =
    OASISSection_intern.section_fields
      (fun () -> (s_ "library"))
      schm
      (fun (cs, _, _) -> cs)
  in
  let build_section_gen =
    OASISBuildSection_intern.section_fields
      (fun () -> (s_ "library"))
      Best
      schm
      (fun (_, bs, _) -> bs)
  in
  let external_modules =
    new_field schm "Modules"
      ~default:[]
      ~quickstart_level:Beginner
      modules
      (fun () ->
         s_ "List of modules to compile.")
      (fun (_, _, lib) -> lib.lib_modules)
  in
  let internal_modules =
    new_field schm "InternalModules"
      ~default:[]
      ~quickstart_level:Beginner
      modules
      (fun () ->
         s_ "List of modules to compile which are not exported.")
      (fun (_, _, lib) -> lib.lib_internal_modules)
  in
  let pack =
    new_field schm "Pack"
      ~default:false
      ~quickstart_level:Expert
      ~feature:OASISFeatures.pack
      boolean
      (fun () ->
         s_ "Set if we should build a packed library.")
      (fun (_, _, lib) -> lib.lib_pack)
  in
  let findlib_parent =
    new_field schm "FindlibParent"
      ~default:None
      (opt internal_library)
      (fun () ->
         s_ "Library which includes the current library. The current library \
             will be built as its parents and installed along it.")
      (fun (_, _, lib) -> lib.lib_findlib_parent)
  in
  let findlib_name =
    new_field schm "FindlibName"
      ~default:None
      (* TODO: Check that the name is correct if this value is None, the
               package name must be correct
      *)
      (opt findlib_name)
      (fun () ->
         s_ "Name used by findlib.")
      (fun (_, _, lib) -> lib.lib_findlib_name)
  in
  let findlib_containers =
    new_field schm "FindlibContainers"
      ~default:[]
      (* TODO: check that a container doesn't overwrite a real package
      *)
      (dot_separated string_not_empty)
      (fun () ->
         s_ "Virtual containers for sub-package, dot-separated")
      (fun (_, _, lib) -> lib.lib_findlib_containers)
  in
  schm,
  (fun features_data nm data ->
     Library
       (cmn_section_gen features_data nm data,
        (build_section_gen nm data),
        {
          lib_modules            = external_modules data;
          lib_pack               = pack data;
          lib_internal_modules   = internal_modules data;
          lib_findlib_parent     = findlib_parent data;
          lib_findlib_name       = findlib_name data;
          lib_findlib_containers = findlib_containers data;
        }))
