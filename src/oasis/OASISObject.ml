(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2008-2010, OCamlCore SARL                                    *)
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


open OASISTypes
open OASISGettext


let source_unix_files ~ctxt (cs, bs, obj) source_file_exists =
  List.fold_left
    (fun acc modul ->
       match OASISLibrary.find_module source_file_exists bs modul with
         | `Sources (base_fn, lst) ->
             (base_fn, lst) :: acc
         | `No_sources _ ->
             OASISMessage.warning
               ~ctxt
               (f_ "Cannot find source file matching \
                    module '%s' in object %s")
               modul cs.cs_name;
             acc)
    []
    obj.obj_modules


let generated_unix_files
      ~ctxt
      ~is_native
      ~source_file_exists
      (cs, bs, obj) =

  let find_module ext modul =
    match OASISLibrary.find_module source_file_exists bs modul with
      | `Sources (base_fn, _) -> [base_fn ^ ext]
      | `No_sources lst ->
        OASISMessage.warning
          ~ctxt
          (f_ "Cannot find source file matching \
               module '%s' in object %s")
          modul cs.cs_name ;
        lst
  in

  let header, byte, native, c_object, f =
    match obj.obj_modules with
      | [ m ] -> (find_module ".cmi" m,
                  find_module ".cmo" m,
                  find_module ".cmx" m,
                  find_module ".o" m,
                  fun x -> x)
      | _ -> ([cs.cs_name ^ ".cmi"],
              [cs.cs_name ^ ".cmo"],
              [cs.cs_name ^ ".cmx"],
              [cs.cs_name ^ ".o"],
              OASISUnixPath.concat bs.bs_path)
  in
    List.map (List.map f) (
      match bs.bs_compiled_object with
        | Native ->
            native :: c_object :: byte :: header :: []
        | Best when is_native ->
            native :: c_object :: byte :: header :: []
        | Byte | Best ->
            byte :: header :: [])


(* END EXPORT *)


open OASISSchema_intern
open PropList.Field
open OASISValues


let schema, generator =
  let schm =
    schema "Object" (fun (cs, _, _) -> cs.cs_plugin_data)
  in
  let cmn_section_gen =
    OASISSection_intern.section_fields
      (fun () -> (s_ "object"))
      schm
      (fun (cs, _, _) -> cs)
  in
  let build_section_gen =
    OASISBuildSection_intern.section_fields
      (fun () -> (s_ "object"))
      Best
      schm
      (fun (_, bs, _) -> bs)
  in
  let modules =
    new_field schm "Modules"
      ~default:[]
      ~quickstart_level:Beginner
      modules
      (fun () ->
         s_ "List of modules to compile.")
      (fun (_, _, obj) -> obj.obj_modules)
  in
  let findlib_fullname =
    new_field schm "FindlibFullName"
      ~default:None
      (* TODO: Check that the name is correct if this value is None, the
               package name must be correct
       *)
      (opt (dot_separated findlib_name))
      (fun () ->
         s_ "Name used by findlib.")
      (fun (_, _, obj) -> obj.obj_findlib_fullname)
  in
    schm,
    (fun oasis_version nm data ->
       Object
         (cmn_section_gen oasis_version nm data,
          (build_section_gen nm data),
          {
            obj_modules            = modules data;
            obj_findlib_fullname   = findlib_fullname data;
          }))

