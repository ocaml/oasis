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


(** [source_unix_files (cs, bs, obj) source_file_exists] Source files for this
    object. The first part of the tuple is the file without extenstion for
    modules and the second part is the source files matching (e.g. .ml and
    .mli).
*)
val source_unix_files:
  ctxt:OASISContext.t ->
  common_section * build_section * object_ ->
  (unix_filename -> bool) ->
  (unix_filename * (unix_filename list)) list


(** [generated_unix_files ~ctxt source_file_exists has_native_dynlink
    is_native ext_lib ext_dll (cs, bs, lib)]
    Compute all files expected by a build of the library. For each file a list
    of alternatives is provided.
*)
val generated_unix_files:
  ctxt:OASISContext.t ->
  is_native:bool ->
  source_file_exists:(unix_filename -> bool) ->
  common_section * build_section * object_ ->
  unix_filename list list


(** Schema for the section. {b Not exported}.
*)
val schema: (common_section * build_section * object_) OASISSchema.t
