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


(** Various utilities
    @author Sylvain Le Gall
*)


(** {2 Map} *)

module MapExt:
sig
  module type S =
  sig
    include Map.S

    (** Extends a map with an association list. *)
    val add_list: 'a t -> (key * 'a) list -> 'a t

    (** Convert an association list to a map. *)
    val of_list: (key * 'a) list -> 'a t

    (** Convert a map to an association list. *)
    val to_list: 'a t -> (key * 'a) list
  end

  module Make: functor (Ord: Map.OrderedType) -> S with type key = Ord.t
end

module MapString: MapExt.S with type key = String.t


(** {2 Set} *)

module SetExt:
sig
  module type S =
  sig
    include Set.S

    (** Extends a set with a list. *)
    val add_list: t -> elt list -> t

    (** Convert a list to a set. *)
    val of_list: elt list -> t

    (** Shortcut for [Set.elements]. *)
    val to_list: t -> elt list
  end

  module Make: functor (Ord: Set.OrderedType) -> S with type elt = Ord.t
end


(** Set for String. *)
module SetString: SetExt.S with type elt = String.t


(** Set for String. *)
module SetStringCsl: SetExt.S with type elt = String.t


(** {2 Hashtable} *)


(** Caseless string hashtable
*)
module HashStringCsl: Hashtbl.S with type key = String.t


(** {2 Variable name} *)


(** [varname_of_string ~hyphen:c s] Transform a string [s] into a variable name,
    following this convention: no digit at the beginning, lowercase, only a-z
    and 0-9 chars. Whenever there is a problem, use an hyphen.
*)
val varname_of_string: ?hyphen:char -> string -> string


(** [varname_concat ~hyphen p s] Concat variable name, removing hyphen at end
    of [p] and at beginning of [s].
*)
val varname_concat: ?hyphen:char -> string -> string -> string


(** [is_varname str] Check that the string [str] is a valid varname. See
    {!varname_of_string} for definition.
*)
val is_varname: string -> bool


(** {2 Fail with Printf.sprintf} *)


(** This function raise the [Failure] exception just as [failwith]
    except that one specify the string raised through a format string.

    Example: [failwithf "Cannot do %s because of %d" str i]
*)
val failwithf: ('a, unit, string, 'b) format4 -> 'a


(** {2 String} *)


(** Caseless compare function
*)
val compare_csl: string -> string -> int


(** {2 Options} *)

(** [may f (Some x)] calls [f x] or do nothing. *)
val may: ('a -> unit) -> 'a option -> unit

module POSIXShell:
sig
  (** [split s]: the string [s] is interpreted as command line
      arguments and splitted into its components (un-escaped).  For
      example [split "a \"b c\" d" = ["a"; "b c"; "d"]].  Note that
      [split "" = []].  It is possible that substitutions such as "$a"
      (resp. "$(a b)") may be transformed into "$\{a\}" (resp. "$\{a b\}"). *)
  val split: string -> string list

  (** [escape s] quote [s] if needed to protect spaces, '"' and '\''
      so it reads as a single argument in a POSIX shell command, the
      content of which is identical to [s] (interpreted with OCaml
      conventions).  If quoted, the returned string will start and end
      with '"'.  The original string [s] is returned if no quoting is
      necessary. *)
  val escape: string -> string

  (** [unescape s] returns a string [s'] removing all backslashes
      preceding a char. *)
  val unescape: string -> string
end

