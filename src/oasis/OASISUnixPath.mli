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
val current_dir_name: unix_filename


(** Test if the filename is current dir (either '.' or '') on Unix. *)
val is_current_dir: unix_filename -> bool


(** '..' on Unix. *)
val parent_dir_name: unix_filename


(** [concat fn1 fn2] Concatenate fn1 and fn2, i.e. [fn1^'/'^fn2]. *)
val concat: unix_filename -> unix_filename -> unix_filename


(** [make lst] Concatenate all filename components of [lst]. *)
val make: unix_filename list -> unix_filename


(** [dirname fn] Return directory name of [fn] or [current_dir_name] if no
    directory name is defined.
*)
val dirname: unix_filename -> unix_filename


(** [basename fn] Return filename without its directory name.
*)
val basename: unix_filename -> unix_filename


(** [chop_extension fn] Remove the last part of the filename, after a '.',
    return [fn] if there is no extension.
*)
val chop_extension: unix_filename -> unix_filename


(** [check_extension fn ext] Check that the filen [fn] has the extension [ext].
    {b Not exported}
*)
val check_extension: unix_filename -> string -> bool


(** [add_extension fn ext] Add the extension [ext] to the filename [fn].
    {b Not exported}
*)
val add_extension: unix_filename -> string -> unix_filename


(** [replace_extension fn ext] Add the extension [ext] to the filename [fn].
    {b Not exported}
*)
val replace_extension: unix_filename -> string -> unix_filename


(** [capitalize_file fn] Return filename capitalized.
*)
val capitalize_file: unix_filename -> unix_filename


(** [uncapitalize_file fn] Return filename uncapitalized.
*)
val uncapitalize_file: unix_filename -> unix_filename


(** Try to compress the filename by removing '.' and collapsing '..'.
    {b Not exported}
*)
val reduce: unix_filename -> unix_filename


(** [make_relative fn_root fn] Make [fn] relative to [fn_root].
    {b Not exported}
*)
val make_relative: unix_filename -> unix_filename -> unix_filename


(** Test if the filename is the current directory.
    {b Not exported}
*)
val is_current: unix_filename -> bool


(** Set for Unix path.
    {b Not exported}
*)
module Set: OASISUtils.SetExt.S with type elt = unix_filename
