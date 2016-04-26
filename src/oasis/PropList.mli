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


(** Property list
    @author Sylvain Le Gall
*)


(** {2 Types and exceptions} *)


type name = string


(** The field of [name] is not set with optional explanation.
*)
exception Not_set of name * string option


(** Can retrieve the field value, but no printer can convert.
    it to string.
*)
exception No_printer of name


(** [Unknown_field (fld, schm)] Unknown field [fld] in schema [schm].
*)
exception Unknown_field of name * name


(** {2 Modules} *)


(** This module stores heterogeneous data defined in Schema and Field.
*)
module Data:
sig

  type t

  (** Create a data storage. *)
  val create: unit -> t

  (** Clear a data storage. *)
  val clear: t -> unit

  (** List field set, not exported *)
  val elements: t -> string list

  (** Dump Data.t to ODN, not exported. *)
  val odn_of_t: t -> ODN.t
end


(** This module is a set of fields (Field.t and FieldRO.t) that can be
    addressed by their name (as string). Value can be set and retrieved
    as string only. However, the value itself is stored in its native
    type.
*)
module Schema:
sig
  (** A value. *)
  type ('a, 'b) value

  (** A schema. *)
  type ('a, 'b) t

  (** Create a schema. *)
  val create: ?case_insensitive:bool -> name -> ('a, 'b) t

  (** Check that the given field name exists. *)
  val mem: ('a, 'b) t -> name -> bool

  (** [get t data nm] Retrieve the string value of field [nm] from schema [t]
      stores in [data].
  *)
  val get: ('a, 'b) t -> Data.t -> name -> string

  (** [set t data nm ~context str] Parse string value [str] in [~context] and
      stores it in [data] for field [nm] of schema [t].
  *)
  val set: ('a, 'b) t -> Data.t -> name -> ?context:'a -> string -> unit

  (** [fold f acc t] Apply [f acc field_name field_extra field_help] in turn
      to all fields of schema [t].
  *)
  val fold:
    ('a -> name -> 'b -> (unit -> string) option -> 'a) ->
    'a -> ('c, 'b) t -> 'a

  (** Same as {!Schema.fold} except no accumulator are involved. *)
  val iter:
    (name -> 'a -> (unit -> string) option -> unit) -> ('b, 'a) t -> unit

  (** Get the name of the schema. *)
  val name: ('a, 'b) t -> name
end


(** This module defines a field that hold a value. A field can be set and
    retrieve. It is stored in {!Data.t}.
*)
module Field:
sig
  (** A field. *)
  type ('a, 'b, 'c) t

  (** Create a field, and optionally attached it to a schema.
  *)
  val create:
    ?schema:('a, 'b) Schema.t ->
    ?name:name ->
    ?parse:(?context:'a -> string -> 'c) ->
    ?print:('c -> string) ->
    ?default:'c ->
    ?update:(?context:'a -> 'c -> 'c -> 'c) ->
    ?help:(unit -> string) -> 'b -> ('a, 'c, 'b) t

  (** Store a field in a {!Data.t}. *)
  val fset: Data.t -> ('a, 'b, 'c) t -> ?context:'a -> 'b -> unit

  (** Retrieve a field from a {!Data.t}. *)
  val fget: Data.t -> ('a, 'b, 'c) t -> 'b

  (** Same as {!Field.fset} but parses a string to get the value. *)
  val fsets: Data.t -> ('a, 'b, 'c) t -> ?context:'a -> string -> unit

  (** Same as {!Field.fget} but applies a printer to the value returned. *)
  val fgets: Data.t -> ('a, 'b, 'c) t -> string
end


(** This module defines a read-only field. However, it can only be set through
    {!Schema.set} if the field is attached to a schema.
*)
module FieldRO:
sig

  (** Create a read-only field. The returned function can be used to retrieve
      the value of the field.
  *)
  val create:
    ?schema:('a, 'b) Schema.t ->
    ?name:name ->
    ?parse:(?context:'a -> string -> 'c) ->
    ?print:('c -> string) ->
    ?default:'c ->
    ?update:(?context:'a -> 'c -> 'c -> 'c) ->
    ?help:(unit -> string) -> 'b -> Data.t -> 'c
end
