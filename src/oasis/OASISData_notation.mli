(******************************************************************************)
(* ocaml-data-notation: Store data using OCaml notation                       *)
(*                                                                            *)
(* Copyright (C) 2009-2011, OCamlCore SARL                                    *)
(* Copyright (C) 2013, Sylvain Le Gall                                        *)
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

(** OCaml data notation.

    This module helps to translate OCaml data into a string following
    OCaml syntax.
  *)

(** {2 Types} *)

type module_name  = string
type field_name   = string
type variant_name = string
type var_name     = string

type t =
  (** Record *)
  | REC of module_name * (field_name * t) list
  (** List *)
  | LST of t list
  (** String *)
  | STR of string
  (** Variant type constructor *)
  | VRT of variant_name * t list
  (** Boolean *)
  | BOO of bool
  (** Integer *)
  | INT of int
  (** Float *)
  | FLT of float
  (** Tuple *)
  | TPL of t list
  (** Unit () *)
  | UNT
  (** Function application *)
  | APP of var_name * (var_name * t) list * t list
  (** Variable *)
  | VAR of var_name
  (** Polymorphic variant *)
  | PVR of variant_name * t option

type 'a conv = 'a -> t

val unit : unit conv
val bool : bool conv
val string : string conv
val int : int conv
val float : float conv
val option : 'a conv -> 'a option conv
val list : 'a conv -> 'a list conv
val tuple2 : 'a conv -> 'b conv -> ('a * 'b) conv
val tuple3 : 'a conv -> 'b conv -> 'c conv -> ('a * 'b * 'c) conv
val tuple4 :
    'a conv -> 'b conv -> 'c conv -> 'd conv -> ('a * 'b * 'c * 'd) conv
val tuple5 :
    'a conv ->  'b conv -> 'c conv -> 'd conv -> 'e conv ->
      ('a * 'b * 'c * 'd * 'e) conv

val vrt0 : string -> t
val vrt1 : 'a conv -> string -> 'a -> t
val vrt2 : 'a conv -> 'b conv -> string -> 'a -> 'b -> t
val vrt3 : 'a conv -> 'b conv -> 'c conv -> string -> 'a -> 'b -> 'c -> t

(** {2 Functions} *)

(** Function that can be generated using ODN
    func_call = APP(func, [], [func_arg]). *)
type 'a func =
  { func_call: 'a;
    func_name: string;
    func_arg:  t option;
  }


(** Return the OCaml function corresponding to a [func]. *)
val func: 'a -> string -> 'a func

(** Create a func with an argument *)
val func_with_arg: ('a -> 'b) -> string -> 'a -> ('a -> t) -> 'b func


(** Return the [t] code corresponding to a [func]. *)
val serialize_func: 'a func -> t

(** Return the OCaml function corresponding to a [func]. *)
val func_call: 'a func -> 'a

(** {2 Formatting} *)

type 'a printer = Format.formatter -> 'a -> unit

val pp : ?opened_modules:string list -> t printer
val to_string : ?opened_modules:string list -> t -> string

