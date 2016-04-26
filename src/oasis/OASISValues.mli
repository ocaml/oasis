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


(** Parse, print and check values

    This module allows to parse values that should match a particular content
    (URL, list).

    The whole module is {b not exported}.

    @author Sylvain Le Gall
*)


(** {2 Types and exception} *)


(** Definition of a value. *)
type 'a t =
  {
    parse: ctxt:OASISContext.t -> string -> 'a;
    (** Parse a string into value *)
    update: 'a -> 'a -> 'a;
    (** Merge two values into one *)
    print: 'a -> string;
    (** Convert a value to string *)
  }


(** The value exist but there is no easy way to represent it.
*)
exception Not_printable


(** It is not possible to combine values.
*)
exception Not_combinable


(** Always raise {!Not_combinable}.
*)
val update_fail: 'a -> 'b -> 'c


(** {2 Basic values and combinators} *)


(** Hidden value to build phantom data storage, cannot set/get
    it using string.
*)
val blackbox: 'a t


(** String value. *)
val string: string t


(** String value, must not be "". *)
val string_not_empty: string t


(** Boolean value, use [bool_of_string] to parse. *)
val boolean: bool t


(** Extra check to see if the string value, can be expanded
    using [Buffer.add_substitute] rules.
*)
val expandable: string t -> string t


(** [dot_separated v] When parsing split the input string using '.' separator
    and apply [v.parse]. Merge by concatenate two values, and print by joining
    [v.print] generated strings using a '.' separator. Don't strip whitespaces.
*)
val dot_separated: 'a t -> 'a list t


(** Same as {!dot_separated} using ',' as separator. Strip whitespaces before
    and after the input string.
*)
val comma_separated: 'a t -> 'a list t


(** Same {!dot_separated} using '\n' as separator. Strip whitespaces before and
    after the input string.
*)
val newline_separated: 'a t -> 'a list t


(** Same as {!dot_separated} using blanks as separator. *)
val space_separated: string list t


(** [with_optional_parentheses v_main v_opt] Combine two values. The input
    string ["abcd (defg)"] is split between the part not between parentheses
    and the one between. [v_main] is applied to the first one and [v_opt] to
    the latter. If no parentheses is found, only apply [v_main].
*)
val with_optional_parentheses: 'a t -> 'b t -> ('a * 'b option) t


(** Optional value. *)
val opt: 'a t -> 'a option t


(** [choices nm lst] Value that must be in a list of predefined choices.
    Find the right association in [lst], comparison is case insensitive.
    If something failed output a message using [nm] as the name of the
    value represented.
*)
val choices: (unit -> string) -> (string * 'a) list -> 'a t


(** {2 Standard values} *)


(** URL value. *)
val url: string t


(** Copyright value. *)
val copyright: string t


(** File value. *)
val file: string t


(** File list value. *)
val files: string list t


(** File with glob value. *)
val file_glob: string t


(** Directory value. *)
val directory: string t


(** Module list value. *)
val modules: string list t


(** Category list value. *)
val categories: string list t


(** Findlib package name value, without its path. *)
val findlib_name: string t


(** Findlib package name with path value, e.g. oasis.base. *)
val findlib_full: string t


(** Internal library. *)
val internal_library: string t


(** Command line. *)
val command_line: (string * string list) t


(** Arguments of command line programs.  See {!OASISUtils.POSIX.split}
    for more information. *)
val command_line_options: string list t
