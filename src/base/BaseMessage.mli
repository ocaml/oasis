
(** Message to user
  
    This module is the same as {!OASISMessage}, except we use the
    context {!BaseContext.default}.

    @author Sylvain Le Gall
  *)

(** See {!OASISMessage.debug}.
  *)
val debug : ('a, out_channel, unit, unit) format4 -> 'a

(** See {!OASISMessage.info}.
  *)
val info : ('a, out_channel, unit, unit) format4 -> 'a

(** See {!OASISMessage.warning}.
  *)
val warning : ('a, out_channel, unit, unit) format4 -> 'a

(** See {!OASISMessage.error}.
  *)
val error : ?exit:bool -> ('a, out_channel, unit, unit) format4 -> 'a

(** See {!OASISMessage.string_of_exception}.
  *)
val string_of_exception : exn -> string
