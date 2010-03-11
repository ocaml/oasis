
(** Function compatible with ODN dump
   
    @author Sylvain Le Gall
  *)

(** Function that can be generated using ODN
    func_call = APP(func, [], [func_arg])
  *)
type 'a func =
    {
      func_call: 'a;
      func_name: string;
      func_arg:  ODN.t option;
    }

(** Create a func 
  *)
let func f f_nm = 
  {
    func_call = f;
    func_name = f_nm;
    func_arg  = None;
  }

(** Create a func with an argument
  *)
let func_with_arg f f_nm arg odn_of_arg = 
  {
    func_call = f arg;
    func_name = f_nm;
    func_arg  = Some (odn_of_arg arg);
  }

(** Return the ODN.t code corresponding to a func_t
  *)
let odn_of_func t =
  match t.func_arg with 
    | Some arg ->
        ODN.APP (t.func_name, [], [arg])
    | None ->
        ODN.VAR t.func_name

(** Return the OCaml function corresponding to a func_t
  *)
let func_call t =
  t.func_call
