
(** Maintain a log of actions done
    @author Sylvain Le Gall
  *)

open OASISTypes 

(** Default file for registering log.
  *)
val default_filename : host_filename

(** Load the log file.
  *)
val load : unit -> (name * string) list

(** Add an event to the log file.
  *)
val register : name -> string -> unit

(** Remove an event from the log file.
  *)
val unregister : name -> string -> unit

(** Filter events of the log file.
  *)
val filter : name list -> (name * string) list

(** Check if an event exists in the log file.
  *)
val exists : name -> string -> bool
