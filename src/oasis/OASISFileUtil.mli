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


(** File operations
    @author Sylvain Le Gall
*)


open OASISTypes


(** [find_file paths exts] Find a file among all provided [paths], trying
    various extensiosn [exts]. Return the first combination of [paths]
    and [exts].
*)
val find_file:
  ?case_sensitive:bool ->
  host_filename list list ->
  string list ->
  host_filename


(** Find real filename of an executable.
*)
val which: ctxt:OASISContext.t -> host_filename -> host_filename


(** Copy a file.
*)
val cp:
  ctxt:OASISContext.t ->
  ?recurse:bool ->
  host_filename ->
  host_filename ->
  unit


(** Create a directory.
*)
val mkdir: ctxt:OASISContext.t -> host_filename -> unit


(** [mkdir_parent f tgt] Create a directory and its parent, call f with
    directory name created, in order.
*)
val mkdir_parent:
  ctxt:OASISContext.t -> (host_filename -> 'a) -> host_filename -> unit


(** Remove a directory.
*)
val rmdir: ctxt:OASISContext.t -> host_filename -> unit


(** Expand a filename containing '*.ext' into corresponding
    real files.
*)
val glob: ctxt:OASISContext.t -> string -> host_filename list


(** Test file existence, considering case even on case insensitive filesystem.
*)
val file_exists_case: string -> bool
