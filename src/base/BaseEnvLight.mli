
(** Read-only access to 'setup.data'
  
    This module defines the minimal set of functions to access data 
    contained in 'setup.data'. It allows to build third party OCaml
    script, without embedding hundreeds line of code.

    @author Sylvain Le Gall
  *)

module MapString: Map.S with type key = string

type t = string MapString.t

(** Environment default file 
  *)
val default_filename : string

(** Load environment.
  *)
val load : ?allow_empty:bool -> ?filename:string -> unit -> t

(** Get a variable that evaluate expression that can be found in it (see
    [Buffer.add_substitute]).
  *)
val var_get : string -> t -> string
