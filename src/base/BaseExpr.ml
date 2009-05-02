
(** Conditional expression like in OASIS.
    @author Sylvain Le Gall
  *)

open BaseEnvironment;;

type t =
  | Bool of bool
  | Not of t
  | And of t * t
  | Or of t * t
  | Flag of string
  | Test of string * string
;;

type 'a choices = (t * 'a) list
;;

(** Evaluate expression *)
let rec eval env =
  function
    | Bool b ->
        b, env
    | Not e -> 
        let v, env =
          eval env e 
        in
          not v, env
    | And (e1, e2) ->
        let (v1, env) as res =
          eval env e1
        in
          if v1 then
            eval env e2
          else
            res
    | Or (e1, e2) -> 
        let (v1, env) as res =
          eval env e1
        in
          if not v1 then
            eval env e2
          else
            res
    | Flag nm ->
        let v, env =
          var_get nm env
        in
        let v, env =
          var_expand v env
        in
          assert(v = "true" || v = "false");
          (v = "true"), env
    | Test (nm, vl) ->
        let v, env =
          var_get nm env
        in
        let v, env =
          var_expand v env
        in
          (v = vl), env
;;

let choose lst env =
  let res, env =
    List.fold_left
      (fun (res, env) (cond, vl) ->
         let res_cond, env =
           eval env cond
         in
           (if res_cond then Some vl else res), env)
      (None, env)
      lst
  in
    match res with 
      | Some vl ->
          vl, env
      | None ->
          failwith "No result for a choice list"
;;
