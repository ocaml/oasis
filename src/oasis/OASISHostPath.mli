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

(** Sys.os_type = "Win32"
*)
val os_type_windows : bool

(**
    use cygwin/msys utils on "Win32"
*)
val use_cygwin: bool

(**
   on windows, the Unix quote function
   will be used, if use_cygwin is true.
   Otherwise identic to Filename.quote
*)
val quote: string -> string

(*
   make sure, that always the right path seperators
   are used.
   Does nothing, Sys.os_type is not "Win32".
   On windows, it enforces uniform path seperators:
   '/' if use_cygwin is true
   '\\' otherwise

   val use_native_path_sep: string -> string
*)


(** Create a filename out of its components.
  *)
val make : host_filename list -> host_filename

(** Convert a unix filename into host filename.
  *)
val of_unix : unix_filename -> host_filename

(** Compare host filename.
    {b Not exported}
  *)
val compare : host_filename -> host_filename -> int

(** See {!OASISUnixPath.add_extension}.
    {b Not exported}
  *)
val add_extension : host_filename -> string -> host_filename



(* moved here to avoid code duplication. See OASISFileUtil *)
val which : host_filename -> host_filename
val file_exists_case : string -> bool
val find_file :
  ?case_sensitive:bool ->
  host_filename list list ->
  string list ->
  host_filename
