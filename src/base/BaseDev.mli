
(** Entry points for 'setup.ml' in dev mode
    @author Sylvain Le Gall
  *)

open OASISTypes

type t = 
    { 
      oasis_cmd : prog; (** Program to regenerate the build system. *)
    }

(** Run 'oasis setup' command line tool to regenerate a build system and 
    and run it.
  *)
val update_and_run : t -> unit

(** Create a 'setup.ml' that calls {!update_and_run}. {b Not exported}.
  *)
val make :
  ?oasis_exec:string ->
  OASISPlugin.context_act -> 
  package -> 
  OASISPlugin.context_act * t

(** Dump [ODN.t]. {b Not exported}.
  *)
val odn_of_t : t -> ODN.t
