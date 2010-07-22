(** Run custom command for pre/post hook
    @author Sylvain Le Gall
  *)

open OASISTypes

(** [run prg args extra_args] Expand and run command. *)
val run : prog -> args -> string array -> unit

(** [hook ~failsafe custom f a] Apply a function nested in a [custom] block 
    as defined by {!OASISTypes.custom}.
  *)
val hook : ?failsafe:bool -> custom -> ('a -> 'b) -> 'a -> 'b
