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

open OASISTypes
open OASISUtils
open OASISGettext

type library_name = name

let generated_unix_files ~ctxt (cs, bs, lib) 
      source_file_exists is_native ext_lib ext_dll =  
  (* The headers that should be compiled along *)
  let headers = 
    List.fold_left
      (fun hdrs modul ->
         try 
           let base_fn = 
             List.find
               (fun fn -> 
                  source_file_exists (fn^".ml") ||
                  source_file_exists (fn^".mli") ||
                  source_file_exists (fn^".mll") ||
                  source_file_exists (fn^".mly")) 
               (List.map
                  (OASISUnixPath.concat bs.bs_path)
                  [modul;
                   String.uncapitalize modul;
                   String.capitalize modul])
           in
             [base_fn^".cmi"] :: hdrs
         with Not_found ->
           OASISMessage.warning
             ~ctxt
             (f_ "Cannot find source file matching \
                  module '%s' in library %s")
             modul cs.cs_name;
             (List.map (OASISUnixPath.concat bs.bs_path)
                [modul^".cmi";
                 String.uncapitalize modul ^ ".cmi";
                 String.capitalize modul ^ ".cmi"])
             :: hdrs)
      []
      lib.lib_modules
  in

  let acc_nopath =
    []
  in

  (* Compute what libraries should be built *)
  let acc_nopath =
    let byte acc =
      [cs.cs_name^".cma"] :: acc
    in
    let native acc =
      [cs.cs_name^".cmxa"] :: [cs.cs_name^(ext_lib ())] :: acc
    in
      match bs.bs_compiled_object with 
        | Native ->
            byte (native acc_nopath)
        | Best when is_native () ->
            byte (native acc_nopath)
        | Byte | Best ->
            byte acc_nopath
  in

  (* Add C library to be built *)
  let acc_nopath = 
    if bs.bs_c_sources <> [] then
      begin
        ["lib"^cs.cs_name^(ext_lib ())]
        ::
        ["dll"^cs.cs_name^(ext_dll ())]
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
      headers


type group_t = 
  | Container of findlib_name * (group_t list)
  | Package of (findlib_name * 
                common_section *
                build_section * 
                library * 
                (group_t list))

let group_libs pkg =
  (** Associate a name with its children *)
  let children =
    List.fold_left
      (fun mp ->
         function
           | Library (cs, bs, lib) ->
               begin
                 match lib.lib_findlib_parent with 
                   | Some p_nm ->
                       begin
                         let children =
                           try 
                             MapString.find p_nm mp
                           with Not_found ->
                             []
                         in
                           MapString.add p_nm ((cs, bs, lib) :: children) mp
                       end
                   | None ->
                       mp
               end
           | _ ->
               mp)
      MapString.empty
      pkg.sections
  in

  (* Compute findlib name of a single node *)
  let findlib_name (cs, _, lib) =
    match lib.lib_findlib_name with 
      | Some nm -> nm
      | None -> cs.cs_name
  in

  (** Build a package tree *)
  let rec tree_of_library containers ((cs, bs, lib) as acc) =
    match containers with
      | hd :: tl ->
          Container (hd, [tree_of_library tl acc])
      | [] ->
          Package 
            (findlib_name acc, cs, bs, lib,
             (try 
                List.rev_map 
                  (fun ((_, _, child_lib) as child_acc) ->
                     tree_of_library 
                       child_lib.lib_findlib_containers
                       child_acc)
                  (MapString.find cs.cs_name children)
              with Not_found ->
                []))
  in

  (** Merge containers with the same name *)
  let rec merge_containers groups =
    (* Collect packages and create the map "container name -> merged children" *)
    let packages, containers =
      List.fold_left
        (fun (packages, containers) group ->
           match group with
             | Container(name, children) ->
                 let children' =
                   try
                     MapString.find name containers
                   with Not_found ->
                     []
                 in
                 (packages,
                  MapString.add name (children' @ children) containers)
             | Package(name, cs, bs, lib, children) ->
                 (Package(name, cs, bs, lib, merge_containers children) :: packages,
                  containers))
        ([], MapString.empty)
        groups
    in
    (* Recreate the list of groups *)
    packages @
      (MapString.fold
         (fun name children acc ->
            Container(name, merge_containers children) :: acc)
         containers [])
  in

    (* TODO: check that libraries are unique *)
    merge_containers
      (List.fold_left
         (fun acc ->
            function
              | Library (cs, bs, lib) when lib.lib_findlib_parent = None -> 
                  (tree_of_library lib.lib_findlib_containers (cs, bs, lib)) :: acc
              | _ ->
                  acc)
         []
         pkg.sections)

(** Compute internal to findlib library matchings, including subpackage
    and return a map of it.
  *)
let findlib_name_map pkg = 

  (* Compute names in a tree *)
  let rec findlib_names_aux path mp grp =
    let fndlb_nm, children, mp =
      match grp with
        | Container (fndlb_nm, children) ->
            fndlb_nm, children, mp
                                  
        | Package (fndlb_nm, {cs_name = nm}, _, _, children) ->
            fndlb_nm, children, (MapString.add nm (path, fndlb_nm) mp)
    in
    let fndlb_nm_full =
      (match path with
         | Some pth -> pth^"."
         | None -> "")^
      fndlb_nm
    in
      List.fold_left
        (findlib_names_aux (Some fndlb_nm_full))
        mp
        children
  in

    List.fold_left
      (findlib_names_aux None)
      MapString.empty
      (group_libs pkg)


let findlib_of_name ?(recurse=false) map nm =
  try 
    let (path, fndlb_nm) = 
      MapString.find nm map
    in
      match path with 
        | Some pth when recurse -> pth^"."^fndlb_nm
        | _ -> fndlb_nm

  with Not_found ->
    failwithf
      (f_ "Unable to translate internal library '%s' to findlib name")
      nm

let name_findlib_map pkg =
  let mp = 
    findlib_name_map pkg
  in
    MapString.fold
      (fun nm _ acc -> 
         let fndlb_nm_full =
           findlib_of_name 
             ~recurse:true 
             mp 
             nm
         in
           MapString.add fndlb_nm_full nm acc)
      mp
      MapString.empty

let findlib_of_group = 
  function
    | Container (fndlb_nm, _) 
    | Package (fndlb_nm, _, _, _, _) -> fndlb_nm

let root_of_group grp =
  let rec root_lib_aux =
    function 
      | Container (_, children) ->
          root_lib_lst children        
      | Package (_, cs, bs, lib, children) ->
          if lib.lib_findlib_parent = None then 
            cs, bs, lib
          else
            root_lib_lst children
  and root_lib_lst =
    function
      | [] ->
          raise Not_found
      | hd :: tl ->
          try
            root_lib_aux hd
          with Not_found ->
            root_lib_lst tl
  in
    try
      root_lib_aux grp
    with Not_found ->
      failwithf
        (f_ "Unable to determine root library of findlib library '%s'")
        (findlib_of_group grp)


(* END EXPORT *)

let schema = OASISLibrary_intern.schema
