
(** Messages to user
  
    These functions display information depending on the verbosity level
    set in {!OASISContext.t}. They use [Printf.fprintf] syntax to output.
    You can use a [~after] function, that will be called if something has
    been displayed.
    
    @author Sylvain Le Gall
  *)

(** Print a debug message.
  *)
val debug :
  ?after:(unit -> unit) ->
  ctxt:OASISContext.t -> ('a, out_channel, unit, unit) format4 -> 'a

(** Print information message.
  *)
val info :
  ?after:(unit -> unit) ->
  ctxt:OASISContext.t -> ('a, out_channel, unit, unit) format4 -> 'a

(** Print a warning message.
  *)
val warning :
  ?after:(unit -> unit) ->
  ctxt:OASISContext.t -> ('a, out_channel, unit, unit) format4 -> 'a

(** Print an error message and exit.
  *)
val error :
  ?after:(unit -> unit) ->
  ?exit:bool ->
  ctxt:OASISContext.t -> ('a, out_channel, unit, unit) format4 -> 'a

(** Convert an exception to a string readable by user. If the appropriate
    printer for the exception cannot be found, use [Printexc.to_string].
  *)
val string_of_exception : exn -> string
