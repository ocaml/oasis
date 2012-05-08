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

(** Library section
    @author Sylvain Le Gall
  *)

open OASISTypes

type library_name = name
type findlib_part_name = name
type 'a map_of_findlib_part_name = 'a OASISUtils.MapString.t

exception InternalLibraryNotFound of library_name
exception FindlibPackageNotFound of findlib_name

(** [source_unix_files (cs, bs, lib) source_file_exists] Source files for this
    library. The first part of the tuple is the file without extenstion for
    modules and the second part is the source files matching (e.g. .ml and
    .mli).
  *)
val source_unix_files :
  ctxt:OASISContext.t ->
  common_section * build_section * library ->
  (unix_filename -> bool) ->
  (unix_filename * (unix_filename list)) list

(** [generated_unix_files ~ctxt source_file_exists has_native_dynlink
    is_native ext_lib ext_dll (cs, bs, lib)]
    Compute all files expected by a build of the library. For each file a list
    of alternatives is provided.
  *)
val generated_unix_files :
  ctxt:OASISContext.t ->
  is_native:bool ->
  has_native_dynlink:bool ->
  ext_lib:string ->
  ext_dll:string ->
  source_file_exists:(unix_filename -> bool) ->
  common_section * build_section * library ->
  unix_filename list list

(** Library groups are organized in trees.
  *)
type group_t =
  | Container of findlib_part_name * group_t list
  | Package of (findlib_part_name *
                common_section * 
                build_section * 
                library * 
                group_t list)

(** Compute groups of libraries, associate root libraries with 
    a tree of its children. A group of libraries is defined by 
    the fact that these libraries have a parental relationship 
    and must be installed together, with the same META file.
  *)
val findlib_mapping: package ->
  group_t list *
  (library_name -> findlib_name) *
  (findlib_name -> library_name)

(** Return the findlib root name of a group, it takes into account
    containers. So the return group name is the toplevel name
    for both libraries and theirs containers.
  *)
val findlib_of_group : group_t -> findlib_name

(** Return the root library, i.e. the first found into the group tree
    that has no parent.
  *)
val root_of_group : group_t -> common_section * build_section * library

(** Schema for the section. {b Not exported}.
  *)
val schema : (common_section * build_section * library) OASISSchema.t
