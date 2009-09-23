
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
        let renv =
          ref env
        in
        let v =
          var_expand renv v
        in
        let env = 
          !renv
        in
          assert(v = "true" || v = "false");
          (v = "true"), env
    | Test (nm, vl) ->
        let v, env =
          var_get nm env
        in
        let renv =
          ref env
        in
        let v =
          var_expand renv v
        in
        let env = 
          !renv
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

(* END EXPORT *)

open OASISTypes;;
open BaseGenCode;;

(** Convert OASIS expression 
  *)
let rec expr_of_oasis =
  function 
    | EBool b -> Bool b
    | ENot e -> Not (expr_of_oasis e)
    | EAnd (e1, e2) -> And(expr_of_oasis e1, expr_of_oasis e2)
    | EOr (e1, e2) -> Or(expr_of_oasis e1, expr_of_oasis e2)
    | EFlag s -> Flag s
    | ETest (s1, s2) -> Test (s1, s2)
;;

(** Convert an OASIS choice list to BaseExpr.choices
  *)
let choices_of_oasis lst =
  List.map
    (fun (e, v) -> expr_of_oasis e, v)
    lst
;;

(** Convert BaseExpr.t to pseudo OCaml code 
  *)
let rec code_of_expr e =
  let cstr, args =
    match e with 
      | Bool b ->
          "Bool", [BOO b]
      | Not e ->
          "Not", [code_of_expr e]
      | And (e1, e2) ->
          "And", [code_of_expr e1; code_of_expr e2]
      | Or (e1, e2) ->
          "Or", [code_of_expr e1; code_of_expr e2]
      | Flag nm ->
          "Flag", [STR nm]
      | Test (nm, vl) ->
          "Test", [STR nm; STR vl]
  in
    VRT ("BaseExpr."^cstr, args)
;;

(** Convert BaseExpr.choices to pseudo OCaml code
  *)
let code_of_choices code_of_elem lst =
  LST
    (List.map
       (fun (expr, elem) ->
          TPL [code_of_expr expr; code_of_elem elem])
       lst)
;;

(** Convert "bool BaseExpr.choices" to pseudo OCaml code
  *)
let code_of_bool_choices =
  code_of_choices (fun v -> BOO v) 
;;
