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


let schema = OASISObject_intern.schema
