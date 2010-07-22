(** Dump function calls with ODN
   
   The idea of this module is to store a function and its argument with the ODN
   data structure that should be dumped. This allows to type in a way the
   function to be dumped.

   This module is {b not exported}.

   @see <http://forge.ocamlcore.org/projects/odn> OCaml Data Notation project
   @author Sylvain Le Gall
  *)

(** Function that can be generated using ODN
    func_call = APP(func, [], [func_arg]).
  *)
type 'a func = 
    {
      func_call: 'a;
      func_name: string;
      func_arg:  ODN.t option;
    }

(** Return the OCaml function corresponding to a [func].
  *)
val func: 'a -> string -> 'a func

(** Create a func with an argument
  *)
val func_with_arg: ('a -> 'b) -> string -> 'a -> ('a -> ODN.t) -> 'b func

(** Return the [ODN.t] code corresponding to a [func].
  *)
val odn_of_func: 'a func -> ODN.t

(** Return the OCaml function corresponding to a [func].
  *)
val func_call: 'a func -> 'a
