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

(** Manipulate host filenames
    @author Sylvain Le Gall
  *)

open OASISTypes

(** See {!OASISUnixPath}. *)
module Unix:  
sig
  val concat : unix_filename -> unix_filename -> unix_filename
  val make : unix_filename list -> unix_filename
  val dirname : unix_filename -> unix_filename
  val basename : unix_filename -> unix_filename
  val chop_extension : unix_filename -> unix_filename
end

(** Create a filename out of its components.
  *)
val make : host_filename list -> host_filename

(** Convert a unix filename into host filename.
  *)
val of_unix : unix_filename -> host_filename
