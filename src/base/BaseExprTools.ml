
(** Tools to manipulate BaseExpr
    @author Sylvain Le Gall
  *)

open BaseExpr;;
open OASISTypes;;
open Format;;
open BaseUtils;;

(** Convert OASIS expression 
  *)
let rec of_oasis =
  function 
    | EBool b -> Bool b
    | ENot e -> Not (of_oasis e)
    | EAnd (e1, e2) -> And(of_oasis e1, of_oasis e2)
    | EOr (e1, e2) -> Or(of_oasis e1, of_oasis e2)
    | EFlag s -> Flag s
    | ETest (s1, s2) -> Test (s1, s2)
;;

(** Convert an OASIS choice list to BaseExpr
  *)
let of_oasis_choices lst =
  List.map
    (fun (e, v) -> of_oasis e, v)
    lst
;;

(** Pretty BaseExpr code 
  *)
let rec pp_code_expr fmt =
  function
    | Bool b ->
        fprintf fmt "BaseExpr.Bool %b" b
    | Not e ->
        fprintf fmt "@[<hv2>BaseExpr.Not@ (@[%a@])@]" pp_code_expr e 
    | And (e1, e2) ->
        fprintf fmt "@[<hv2>BaseExpr.And@ (@[%a,@ %a@])@]" pp_code_expr e1 pp_code_expr e2
    | Or (e1, e2) ->
        fprintf fmt "@[<hv2>BaseExpr.Or@ (@[%a,@ %a@])@]" pp_code_expr e1 pp_code_expr e2
    | Flag nm ->
        fprintf fmt "BaseExpr.Flag %S" nm
    | Test (nm, vl) ->
        fprintf fmt "@[<hv2>BaseExpr.Test (@[%S,@ %S@])@]" nm vl
;;

(** Pretty print conditional list 
  *)
let pp_code_expr_choices pp_elem =
  pp_ocaml_list
    (fun fmt (cond, e) ->
       fprintf fmt "%a,@ %a"
         pp_code_expr cond
         pp_elem  e)
;;

