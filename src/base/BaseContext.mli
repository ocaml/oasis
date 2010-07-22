
(** Global context for messages and i18n
  
    This module is the same as {!OASISContext}. It is only aliases.

    @author Sylvain Le Gall
  *)


(** See {!OASISContext.args}.
  *)
val args : unit -> (string * Arg.spec * string) list

(** See {!OASISContext.default}
  *)
val default : OASISContext.t ref
