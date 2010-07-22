
(** Handle output of 'ocamlc -config'
  
   Read output of command ocamlc -config and transform it
   into environment variables.

   @author Sylvain Le Gall
  *)

open OASISTypes

(** The ocamlc command line tool. Use {!BaseStandardVar.ocamlc} variable if you
    want a safe way to access this variable.
  *)
val ocamlc : unit -> string

(** Look for the variable name in the 'ocamlc -config' output and define
    it.
  *)
val var_define : name -> (unit -> string)
