(** Handle command line argument
    @author Sylvain Le Gall
  *)

(** Parse command line argument, using provided arguments. Works like
    [Arg.parse_argv].
  *)
val parse : string array -> (Arg.key * Arg.spec * Arg.doc) list -> unit
