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

(** SourceRepository section
    @author Sylvain Le Gall
  *)

open OASISTypes

(** [generated_unix_files ~ctxt (cs, bs, lib) source_file_exists is_native ext_lib ext_dll] 
    Compute all files expected by a build of the library. For each file a list
    of alternatives is provided.
  *)
val generated_unix_files :
  ctxt:OASISContext.t ->
  common_section * build_section * library ->
  (string -> bool) ->
  (unit -> bool) ->
  (unit -> string) -> (unit -> string) -> unix_filename list list

(** Library groups are organized in trees.
  *)
type group_t =
  | Container of findlib_name * group_t list
  | Package of (findlib_name * 
                common_section * 
                build_section * 
                library * 
                group_t list)

(** Compute groups of libraries, associate root libraries with 
    a tree of its children. A group of libraries is defined by 
    the fact that these libraries have a parental relationship 
    and must be installed together, with the same META file.
  *)
val group_libs : package -> group_t list

type library_name = name

(** Compute internal to findlib library matchings, including subpackage
    and return a map of it.
  *)
(* TODO: string option for optional parent path seem strange, try to get
 * rid of it
 *)
val findlib_name_map :
  package ->
  (string option * findlib_name) OASISUtils.MapString.t

(** Return the findlib name of the library without parents *)
(* TODO: is ~recurse really mandatory, try to get rid of it
 *)
val findlib_of_name :
  ?recurse:bool ->
  (string option * findlib_name) OASISUtils.MapString.t ->
  library_name -> string

(** Compute findlib to internal library matching. 
  *)
val name_findlib_map :
  package -> library_name OASISUtils.MapString.t

(** Return the findlib root name of a group, it takes into account
    containers. So the return group name is the toplevel name
    for both libraries and theirs containers.
  *)
val findlib_of_group : group_t -> findlib_name

(** Return the root library, i.e. the first found into the group tree
    that has no parent.
  *)
val root_of_group :
  group_t ->
  common_section * build_section * library

(** Schema for the section. {b Not exported}.
  *)
val schema : (common_section * build_section * library) OASISSchema.t
