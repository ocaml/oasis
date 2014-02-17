
open CLISubCommand

(** Add arguments to define where is located '_oasis'. *)
val define_oasis_fn:
  ((OASISTypes.host_filename -> 'a) run_t) -> ('a run_t)

(** Locate '_oasis' and parse it. *)
val parse_oasis_fn:
  ((OASISTypes.host_filename -> OASISTypes.package -> 'a) run_t) -> ('a run_t)
