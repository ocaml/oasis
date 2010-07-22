
(** Boolean expressions

    This module provides a boolean expression evaluator. See OASIS manual 
    for the precise syntax of the boolean expression

    @author Sylvain Le Gall
    @see <http://oasis.forge.ocamlcore.org/MANUAL.html#conditional-value> OASIS Manual 
  *)

(** {2 Test} *)

(** Test definition.
  *)
type test = 
  | TOs_type
  | TSystem
  | TArchitecture
  | TCcomp_type
  | TOCaml_version
  
(** All tests. *)
val tests : test list

(** Convert a test to string. *)
val string_of_test : test -> string

(** Convert a string to test. *)
val test_of_string : string -> test


(** {2 Expression} *)

type flag = string

(** Boolean expression definition.
  *)
type t =
  | EBool of bool  (** true or false *)
  | ENot of t      (** ! e *)
  | EAnd of t * t  (** e1 && e2 *)
  | EOr of t * t   (** e1 || e2 *)
  | EFlag of flag  (** flag(foo) *)
  | ETest of test * string (** os_type(Win32) *)
  
(** [choose ~printer ~name eval_flg eval_tst choices] Evaluate each conditions
    of [choices] and choose the last condition that evaluates to [true].
    Use [eval_flg] and [eval_tst] to get values of flags and tests. If 
    something goes wrong, use [printer] to display values and [~name] as
    the choice list name.
  *)
val choose :
  ?printer:('a -> string) ->
  ?name:string ->
  (string -> string) ->
  (test -> string) -> (t * 'a) list -> 'a

(** Check that a boolean expression only use available flags. {b Not exported}.
  *)
val check : (flag list) -> t -> unit

(** Try to reduce the size of a boolean expression. {b Not exported}.
  *)
val reduce : t -> t

(** Try to reduce the size of a choice list. {b Not exported}.
  *)
val reduce_choices : (t * 'a) list -> (t * 'a) list

(** Dump ODN.t. {b Not exported}. *)
val odn_of_t: t -> ODN.t
