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

(** Unix path manipulation 
  
  The filename and dirname used in '_oasis' file and {!OASISTypes.package} are
  always encoded as Unix path. They are changed when using it on the target 
  system.

  @author Sylvain Le Gall
  *)

type unix_filename = string
type unix_dirname = unix_filename
type host_filename = string
type host_dirname = host_filename

(** '.' on Unix. *)
val current_dir_name : unix_filename

(** '..' on Unix. *)
val parent_dir_name : unix_filename

(** [concat fn1 fn2] Concatenate fn1 and fn2, i.e. [fn1^'/'^fn2]. *)
val concat : unix_filename -> unix_filename -> unix_filename

(** [make lst] Concatenate all filename components of [lst]. *)
val make : unix_filename list -> unix_filename

(** [dirname fn] Return directory name of [fn] or [current_dir_name] if no
    directory name is defined.
  *)
val dirname : unix_filename -> unix_filename

(** [basename fn] Return filename without its directory name.
  *)
val basename : unix_filename -> unix_filename

(** [chop_extension fn] Remove the last part of the filename, after a '.',
    return [fn] if there is no extension.
  *)
val chop_extension : unix_filename -> unix_filename
