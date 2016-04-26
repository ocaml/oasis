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
open OASISUtils
open OASISGettext
open OASISSection


(* Look for a module file, considering capitalization or not. *)
let find_module source_file_exists bs modul =
  let possible_base_fn =
    List.map
      (OASISUnixPath.concat bs.bs_path)
      [modul;
       OASISUnixPath.uncapitalize_file modul;
       OASISUnixPath.capitalize_file modul]
  in
  (* TODO: we should be able to be able to determine the source for every
   * files. Hence we should introduce a Module(source: fn) for the fields
   * Modules and InternalModules
  *)
  List.fold_left
    (fun acc base_fn ->
       match acc with
         | `No_sources _ ->
           begin
             let file_found =
               List.fold_left
                 (fun acc ext ->
                    if source_file_exists (base_fn^ext) then
                      (base_fn^ext) :: acc
                    else
                      acc)
                 []
                 [".ml"; ".mli"; ".mll"; ".mly"]
             in
             match file_found with
               | [] ->
                 acc
               | lst ->
                 `Sources (base_fn, lst)
           end
         | `Sources _ ->
           acc)
    (`No_sources possible_base_fn)
    possible_base_fn


let source_unix_files ~ctxt (cs, bs, lib) source_file_exists =
  List.fold_left
    (fun acc modul ->
       match find_module source_file_exists bs modul with
         | `Sources (base_fn, lst) ->
           (base_fn, lst) :: acc
         | `No_sources _ ->
           OASISMessage.warning
             ~ctxt
             (f_ "Cannot find source file matching \
                  module '%s' in library %s")
             modul cs.cs_name;
           acc)
    []
    (lib.lib_modules @ lib.lib_internal_modules)


let generated_unix_files
    ~ctxt
    ~is_native
    ~has_native_dynlink
    ~ext_lib
    ~ext_dll
    ~source_file_exists
    (cs, bs, lib) =

  let find_modules lst ext =
    let find_module modul =
      match find_module source_file_exists bs modul with
        | `Sources (base_fn, [fn]) when ext <> "cmi"
                                     && Filename.check_suffix fn ".mli" ->
          None (* No implementation files for pure interface. *)
        | `Sources (base_fn, _) ->
          Some [base_fn]
        | `No_sources lst ->
          OASISMessage.warning
            ~ctxt
            (f_ "Cannot find source file matching \
                 module '%s' in library %s")
            modul cs.cs_name;
          Some lst
    in
    List.fold_left
      (fun acc nm ->
         match find_module nm with
           | None -> acc
           | Some base_fns ->
             List.map (fun base_fn -> base_fn ^"."^ext) base_fns :: acc)
      []
      lst
  in

  (* The .cmx that be compiled along *)
  let cmxs =
    let should_be_built =
      match bs.bs_compiled_object with
        | Native -> true
        | Best -> is_native
        | Byte -> false
    in
    if should_be_built then
      if lib.lib_pack then
        find_modules
          [cs.cs_name]
          "cmx"
      else
        find_modules
          (lib.lib_modules @ lib.lib_internal_modules)
          "cmx"
    else
      []
  in

  let acc_nopath =
    []
  in

  (* The headers and annot/cmt files that should be compiled along *)
  let headers =
    let sufx =
      if lib.lib_pack
      then [".cmti"; ".cmt"; ".annot"]
      else [".cmi"; ".cmti"; ".cmt"; ".annot"]
    in
    List.map
      begin
        List.fold_left
          begin fun accu s ->
            let dot = String.rindex s '.' in
            let base = String.sub s 0 dot in
            List.map ((^) base) sufx @ accu
          end
          []
      end
      (find_modules lib.lib_modules "cmi")
  in

  (* Compute what libraries should be built *)
  let acc_nopath =
    (* Add the packed header file if required *)
    let add_pack_header acc =
      if lib.lib_pack then
        [cs.cs_name^".cmi"; cs.cs_name^".cmti"; cs.cs_name^".cmt"] :: acc
      else
        acc
    in
    let byte acc =
      add_pack_header ([cs.cs_name^".cma"] :: acc)
    in
    let native acc =
      let acc =
        add_pack_header
          (if has_native_dynlink then
             [cs.cs_name^".cmxs"] :: acc
           else acc)
      in
      [cs.cs_name^".cmxa"] :: [cs.cs_name^ext_lib] :: acc
    in
    match bs.bs_compiled_object with
      | Native ->
        byte (native acc_nopath)
      | Best when is_native ->
        byte (native acc_nopath)
      | Byte | Best ->
        byte acc_nopath
  in

  (* Add C library to be built *)
  let acc_nopath =
    if bs.bs_c_sources <> [] then
      begin
        ["lib"^cs.cs_name^"_stubs"^ext_lib]
        ::
          ["dll"^cs.cs_name^"_stubs"^ext_dll]
        ::
          acc_nopath
      end
    else
      acc_nopath
  in

  (* All the files generated *)
  List.rev_append
    (List.rev_map
       (List.rev_map
          (OASISUnixPath.concat bs.bs_path))
       acc_nopath)
    (headers @ cmxs)


(* END EXPORT *)


let schema = OASISLibrary_intern.schema

