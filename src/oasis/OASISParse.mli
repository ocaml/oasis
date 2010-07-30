
(** Parse '_oasis' file 

    The whole module is {b not exported}.

    @author Sylvain Le Gall
  *)  

open OASISTypes

(** [from_stream ~ctxt ~ignore_plugins ~fn st] Parse the OASIS file [~fn] and
    check it. If [~ignore_plugins] is set, ignore unknown plugin's fields in the
    file.
  *)
val from_stream: 
  ctxt:OASISContext.t -> 
  ?ignore_plugins:bool -> 
  ?fn:host_filename -> 
  (char Stream.t) ->
  package

(** See {!from_stream}, apply to a filename.
  *)
val from_file :
  ctxt:OASISContext.t -> 
  ?ignore_plugins:bool -> 
  host_filename -> 
  package

(** See {!from_stream}, apply to a string.
  *)
val from_string :
  ctxt:OASISContext.t -> 
  ?ignore_plugins:bool -> 
  ?fn:host_filename -> 
  string ->
  package
