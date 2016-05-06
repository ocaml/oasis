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


(** Boolean expressions

    This module provides a boolean expression evaluator. See OASIS manual
    for the precise syntax of the boolean expression

    @author Sylvain Le Gall
    @see <http://oasis.forge.ocamlcore.org/MANUAL.html#conditional-value> OASIS Manual
*)


(** {2 Test} *)


(** Test definition.
*)
type test


(** Mandatory tests. *)
val tests: test list


(** Convert a test to string. *)
val string_of_test: test -> string


(** Convert a string to test. *)
val test_of_string: string -> test


(** {2 Expression} *)


type flag = string


(** Boolean expression definition.
*)
type t =
  | EBool of bool  (** true or false *)
  | ENot of t      (** ! e *)
  | EAnd of t * t  (** e1 && e2 *)
  | EOr of t * t   (** e1 || e2 *)
  | EFlag of flag  (** flag(foo), a boolean value. *)
  | ETest of test * string (** os_type(Win32), a value compared to a string. *)


(** Choose among different values
*)
type 'a choices = (t * 'a) list


(** [eval eval_tst t] Evaluates the expression. Use [eval_tst]
    to get values of flags and tests.
*)
val eval: (string -> string) -> t -> bool


(** [choose ~printer ~name eval_tst choices] Evaluate each conditions
    of [choices] and choose the last condition that evaluates to [true].
    If something goes wrong, use [printer] to display values and [~name] as the
    choice list name.

    See also {!eval}.
*)
val choose:
  ?printer:('a -> string) ->
  ?name:string ->
  (string -> string) -> 'a choices  -> 'a


(** Check that a boolean expression only use available flags. {b Not exported}.
*)
val check: (flag list) -> t -> unit


(** Try to reduce the size of a boolean expression. {b Not exported}.
*)
val reduce: t -> t


(** Try to reduce the size of a choice list. {b Not exported}.
*)
val reduce_choices: 'a choices -> 'a choices


(** [if_then_else cond choices_if choices_else] Combine choices, if_then_else
    style.
*)
val if_then_else: t -> 'a choices -> 'a choices -> 'a choices

(** Transform an expression into a string. {b Not exported} *)
val to_string: t -> string


(** Transform a list of choice into a string. {b Not exported} *)
val string_of_choices: ('a -> string) -> 'a choices -> string

