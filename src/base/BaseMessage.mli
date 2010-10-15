
(** Message to user
  
    This module is the same as {!OASISMessage}, except we use the
    context {!BaseContext.default}.

    @author Sylvain Le Gall
  *)

(** See {!OASISMessage.debug}.
  *)
val debug : ('a, unit, string, unit) format4 -> 'a

(** See {!OASISMessage.info}.
  *)
val info : ('a, unit, string, unit) format4 -> 'a

(** See {!OASISMessage.warning}.
  *)
val warning : ('a, unit, string, unit) format4 -> 'a

(** See {!OASISMessage.error}.
  *)
val error : ('a, unit, string, unit) format4 -> 'a

(** See {!OASISMessage.string_of_exception}.
  *)
val string_of_exception : exn -> string
