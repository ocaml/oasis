
(** Running commands 
    @author Sylvain Le Gall
  *)

open OASISTypes

(** Run a command.
  *)
val run : ?f_exit_code:(int -> unit) -> prog -> args -> unit

(** Run a command and returns its output.
  *)
val run_read_output : prog -> args -> string list

(** Run a command and returns only first line.
  *)
val run_read_one_line : prog -> args -> string
