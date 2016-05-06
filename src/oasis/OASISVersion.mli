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


(** Version comparisons

    This module handles versions and version comparators. A version is a string
    of the form "1.0.0". We compare integer and non-integer parts between to
    version to order them. Version comparators defined relations to a set
    of version. E.g. ">= 1.0.0" is a version comparator and defines all version
    above "1.0.0", including "1.0.0".

    The version comparison is done using
    {{:http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version}Debian policy for version}.

    @author Sylvain Le Gall
*)


(** {2 Version} *)


type s = string


type t


(** Compare versions.
*)
val version_compare: t -> t -> int


(** Convert and compare version strings.
*)
val version_compare_string: string -> string -> int


(** Convert a string to version.
*)
val version_of_string: string -> t


(** Convert a version to string.
*)
val string_of_version: t -> string


(** Version number value. {b Not exported}.
*)
val value: t OASISValues.t


(** Remove the last part of a version, after the last '.'. I.e. 0.2.0~alpha1 ->
    0.2.
*)
val chop: t -> t

(** {2 Version comparator} *)


type comparator =
  | VGreater of t
  | VGreaterEqual of t
  | VEqual of t
  | VLesser of t
  | VLesserEqual of t
  | VOr of  comparator * comparator
  | VAnd of comparator * comparator


(** Apply version comparator expression.
*)
val comparator_apply: t -> comparator -> bool


(** Convert a comparator to string.  Example of output [">= 3.12.1"].
*)
val string_of_comparator: comparator -> string


(** Convert a comparator to variable name.
*)
val varname_of_comparator: comparator -> string


(** [comparator_ge version comparator]
    Check if [comparator] is compatible with all versions >= than [version]
*)
val comparator_ge: t -> comparator -> bool


(** Convert a string to comparator. {b Not exported}.
*)
val comparator_of_string: string -> comparator


(** Simplify comparator, if possible. {b Not exported}.
*)
val comparator_reduce: comparator -> comparator


(** Check that we have a version constraint. {b Not exported}.
*)
val comparator_value: comparator OASISValues.t

