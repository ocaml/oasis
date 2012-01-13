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

(** File operations
    @author Sylvain Le Gall
  *)

open OASISTypes

(** [find_file paths exts] Find a file among all provided [paths], trying
    various extensiosn [exts]. Return the first combination of [paths]
    and [exts].
  *)
val find_file : host_filename list list -> string list -> host_filename

(** Find real filename of an executable.
  *)
val which : host_filename -> host_filename

(** Copy a file.
  *)
val cp : host_filename -> host_filename -> unit

(** Create a directory.
  *)
val mkdir : host_filename -> unit

(** [mkdir_parent f tgt] Create a directory and its parent, call f with 
    directory name created, in order.
  *)
val mkdir_parent : (host_filename -> 'a) -> host_filename -> unit

(** Remove a directory.
  *)
val rmdir : host_filename -> unit

(** Expand a filename containing '*.ext' into corresponding
    real files.
  *)
val glob : string -> host_filename list
