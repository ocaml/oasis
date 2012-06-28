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
open OASISUtils
open OASISGettext
open OASISSection

type library_name = name
type findlib_part_name = name
type 'a map_of_findlib_part_name = 'a OASISUtils.MapString.t

exception InternalLibraryNotFound of library_name
exception FindlibPackageNotFound of findlib_name

type group_t =
  | Container of findlib_name * group_t list
  | Package of (findlib_name *
                common_section *
                build_section *
                library *
                group_t list)

(* Look for a module file, considering capitalization or not. *)
let find_module source_file_exists (cs, bs, lib) modul =
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
       match find_module source_file_exists (cs, bs, lib) modul with
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
      match find_module source_file_exists (cs, bs, lib) modul with
        | `Sources (base_fn, _) ->
            [base_fn]
        | `No_sources lst ->
            OASISMessage.warning
              ~ctxt
              (f_ "Cannot find source file matching \
                   module '%s' in library %s")
              modul cs.cs_name;
            lst
    in
    List.map 
      (fun nm -> 
         List.map 
           (fun base_fn -> base_fn ^"."^ext)
           (find_module nm))
      lst
  in

  (* The headers that should be compiled along *)
  let headers =
    if lib.lib_pack then
      []
    else
      find_modules
        lib.lib_modules
        "cmi"
  in

  (* The .cmx that be compiled along *)
  let cmxs =
    let should_be_built =
      (not lib.lib_pack) && (* Do not install .cmx packed submodules *)
      match bs.bs_compiled_object with
        | Native -> true
        | Best -> is_native
        | Byte -> false
    in
      if should_be_built then
        find_modules
          (lib.lib_modules @ lib.lib_internal_modules)
          "cmx"
      else
        []
  in

  let acc_nopath =
    []
  in

  (* Compute what libraries should be built *)
  let acc_nopath =
    (* Add the packed header file if required *)
    let add_pack_header acc =
      if lib.lib_pack then
        [cs.cs_name^".cmi"] :: acc
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

type data = common_section * build_section * library
type tree =
  | Node of (data option) * (tree MapString.t)
  | Leaf of data

let findlib_mapping pkg =
  (* Map from library name to either full findlib name or parts + parent. *)
  let fndlb_parts_of_lib_name =
    let fndlb_parts cs lib =
      let name =
        match lib.lib_findlib_name with
          | Some nm -> nm
          | None -> cs.cs_name
      in
      let name =
        String.concat "." (lib.lib_findlib_containers @ [name])
      in
        name
    in
      List.fold_left
        (fun mp ->
           function
             | Library (cs, _, lib) ->
                 begin
                   let lib_name = cs.cs_name in
                   let fndlb_parts = fndlb_parts cs lib in
                     if MapString.mem lib_name mp then
                       failwithf
                         (f_ "The library name '%s' is used more than once.")
                         lib_name;
                     match lib.lib_findlib_parent with
                       | Some lib_name_parent ->
                           MapString.add
                             lib_name
                             (`Unsolved (lib_name_parent, fndlb_parts))
                             mp
                       | None ->
                           MapString.add
                             lib_name
                             (`Solved fndlb_parts)
                             mp
                 end

             | Executable _ | Test _ | Flag _ | SrcRepo _ | Doc _ ->
                 mp)
        MapString.empty
        pkg.sections
  in

  (* Solve the above graph to be only library name to full findlib name. *)
  let fndlb_name_of_lib_name =
    let rec solve visited mp lib_name lib_name_child =
      if SetString.mem lib_name visited then
        failwithf
          (f_ "Library '%s' is involved in a cycle \
               with regard to findlib naming.")
          lib_name;
      let visited = SetString.add lib_name visited in
        try
          match MapString.find lib_name mp with
            | `Solved fndlb_nm ->
                fndlb_nm, mp
            | `Unsolved (lib_nm_parent, post_fndlb_nm) ->
                let pre_fndlb_nm, mp =
                  solve visited mp lib_nm_parent lib_name
                in
                let fndlb_nm = pre_fndlb_nm^"."^post_fndlb_nm in
                  fndlb_nm, MapString.add lib_name (`Solved fndlb_nm) mp
        with Not_found ->
          failwithf
            (f_ "Library '%s', which is defined as the findlib parent of \
                 library '%s', doesn't exist.")
            lib_name lib_name_child
    in
    let mp =
      MapString.fold
        (fun lib_name status mp ->
           match status with
             | `Solved _ ->
                 (* Solved initialy, no need to go further *)
                 mp
             | `Unsolved _ ->
                 let _, mp = solve SetString.empty mp lib_name "<none>" in
                   mp)
        fndlb_parts_of_lib_name
        fndlb_parts_of_lib_name
    in
      MapString.map
        (function
           | `Solved fndlb_nm -> fndlb_nm
           | `Unsolved _ -> assert false)
        mp
  in

  (* Convert an internal library name to a findlib name. *)
  let findlib_name_of_library_name lib_nm =
    try
      MapString.find lib_nm fndlb_name_of_lib_name
    with Not_found ->
      raise (InternalLibraryNotFound lib_nm)
  in

  (* Add a library to the tree.
   *)
  let add sct mp =
    let fndlb_fullname =
      let cs, _, _ = sct in
      let lib_name = cs.cs_name in
        findlib_name_of_library_name lib_name
    in
    let rec add_children nm_lst (children : tree MapString.t) =
      match nm_lst with
        | (hd :: tl) ->
            begin
              let node =
                try
                  add_node tl (MapString.find hd children)
                with Not_found ->
                  (* New node *)
                  new_node tl
              in
                MapString.add hd node children
            end
        | [] ->
            (* Should not have a nameless library. *)
            assert false
    and add_node tl node =
      if tl = [] then
        begin
          match node with
            | Node (None, children) ->
                Node (Some sct, children)
            | Leaf (cs', _, _) | Node (Some (cs', _, _), _) ->
                (* TODO: allow to merge Package, i.e.
                 * archive(byte) = "foo.cma foo_init.cmo"
                 *)
                let cs, _, _ = sct in
                  failwithf
                    (f_ "Library '%s' and '%s' have the same findlib name '%s'")
                    cs.cs_name cs'.cs_name fndlb_fullname
        end
      else
        begin
          match node with
            | Leaf data ->
                Node (Some data, add_children tl MapString.empty)
            | Node (data_opt, children) ->
                Node (data_opt, add_children tl children)
        end
    and new_node =
      function
        | [] ->
            Leaf sct
        | hd :: tl ->
            Node (None, MapString.add hd (new_node tl) MapString.empty)
    in
      add_children (OASISString.nsplit fndlb_fullname '.') mp
  in

  let rec group_of_tree mp =
    MapString.fold
      (fun nm node acc ->
         let cur =
           match node with
             | Node (Some (cs, bs, lib), children) ->
                 Package (nm, cs, bs, lib, group_of_tree children)
             | Node (None, children) ->
                 Container (nm, group_of_tree children)
             | Leaf (cs, bs, lib) ->
                 Package (nm, cs, bs, lib, [])
         in
           cur :: acc)
      mp []
  in

  let group_mp =
    List.fold_left
      (fun mp ->
         function
           | Library (cs, bs, lib) ->
               add (cs, bs, lib) mp
           | _ ->
               mp)
      MapString.empty
      pkg.sections
  in

  let groups =
    group_of_tree group_mp
  in

  let library_name_of_findlib_name =
    Lazy.lazy_from_fun
      (fun () ->
         (* Revert findlib_name_of_library_name. *)
         MapString.fold
           (fun k v mp -> MapString.add v k mp)
           fndlb_name_of_lib_name
           MapString.empty)
  in
  let library_name_of_findlib_name fndlb_nm =
    try
      MapString.find fndlb_nm (Lazy.force library_name_of_findlib_name)
    with Not_found ->
      raise (FindlibPackageNotFound fndlb_nm)
  in

    groups,
    findlib_name_of_library_name,
    library_name_of_findlib_name

let findlib_of_group =
  function
    | Container (fndlb_nm, _)
    | Package (fndlb_nm, _, _, _, _) -> fndlb_nm

let root_of_group grp =
  let rec root_lib_aux =
    (* We do a DFS in the group. *)
    function
      | Container (_, children) ->
          List.fold_left
            (fun res grp ->
               if res = None then
                 root_lib_aux grp
               else
                 res)
            None
            children
      | Package (_, cs, bs, lib, _) ->
          Some (cs, bs, lib)
  in
    match root_lib_aux grp with
      | Some res ->
          res
      | None ->
          failwithf
            (f_ "Unable to determine root library of findlib library '%s'")
            (findlib_of_group grp)

(* END EXPORT *)

let () =
  Printexc.register_printer
    (function
       | InternalLibraryNotFound lib_nm ->
           Some
             (Printf.sprintf
                (f_ "Unable to translate internal library name '%s' \
                     to findlib name.")
                lib_nm)
       | FindlibPackageNotFound fndlb_nm ->
           Some
             (Printf.sprintf
                (f_ "Unable to translate findlib name '%s' \
                     to internal library name.")
                fndlb_nm)
       | _ ->
           None)

let schema = OASISLibrary_intern.schema

