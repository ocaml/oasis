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


(**  Check host system features
     @author Sylvain Le Gall
*)


open OASISTypes


(** Look for a program among a list of alternative program
  * the first found is returned.
*)
val prog_best: prog -> prog list -> unit -> host_filename


(** Check the presence of a particular program.
*)
val prog: prog -> unit -> host_filename


(** Check the presence of a program or its native version.
*)
val prog_opt: prog -> unit -> host_filename


(** The ocamlfind command line tool, used to query version of package.
    Use {!BaseStandardVar.ocamlfind} variable if you want a safe
    way to access this variable.
*)
val ocamlfind: unit -> host_filename


(** [version var_pref cmp ver ()] Check version [ver ()], using [cmp] and
    {!OASISVersion.comparator_apply}. Generate a variable prefixed
    by [var_pref] and using [OASISVersion.varname_of_comparator].
*)
val version:
  string ->
  OASISVersion.comparator -> (unit -> OASISVersion.s) ->
  unit ->
  OASISVersion.s


(** Get findlib package version .
*)
val package_version: findlib_full -> OASISVersion.s


(** Check for findlib package and version. Return install directory.
*)
val package:
  ?version_comparator:OASISVersion.comparator -> findlib_full -> unit ->
  host_dirname
