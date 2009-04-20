
(** OASIS expression manipulation
  *)

open OASISTypes;;
open OASISAstTypes;;

(* Check that expression only use valid tests/flags *)
let check ctxt =
  let lowercase_eq str1 str2 =
    (String.lowercase str1) = (String.lowercase str2)
  in

  let rec check_aux ctxt =
    function
      | EBool _ -> 
          ()
      | ENot e -> 
          check_aux ctxt e 
      | EAnd (e1, e2) | EOr (e1, e2) -> 
          check_aux ctxt e1; 
          check_aux ctxt e2
      | EFlag nm ->
          (
            if not (List.exists (lowercase_eq nm) ctxt.valid_flags) then
              failwith 
                (Printf.sprintf 
                   "Unknown flag '%s'"
                   nm)
          )
      | ETest (nm, vl) ->
          (
            if not (List.exists (lowercase_eq nm) ctxt.valid_tests) then
              failwith 
                (Printf.sprintf 
                   "Unknown test '%s'"
                   nm)
          )
  in
    check_aux ctxt 
;;

(** Reduce expression 
  *)
let rec reduce e =
  let e =
    match e with
      | (EBool _ | EFlag _ | ETest (_, _)) as e ->
          e
      | ENot e ->
          ENot (reduce e)
      | EAnd (e1, e2) ->
          EAnd (reduce e1, reduce e2)
      | EOr (e1, e2) ->
          EOr (reduce e1, reduce e2)
  in
    match e with 
      | EAnd (e, EBool true) | EAnd (EBool true, e)
      | EOr (e, EBool false) | EOr (EBool false, e) ->
          e
      | EAnd (e, EBool false) | EAnd (EBool false, e) ->
          EBool false
      | EOr (e, EBool true) | EOr (EBool true, e) ->
          EBool true
      | ENot (EBool true) ->
          EBool false
      | ENot (EBool false) ->
          EBool true
      | ENot (ENot e) ->
          e
      | (ENot _ | EAnd (_, _) | EOr (_, _) | EFlag _ | ETest (_, _) | (EBool _)) as e ->
          e
;;

(** Reduce choices
  *)
let reduce_choices choices =
  (* Naive reduction, we only look for exactly the same condition in
   * after one condition. It works but is not complete and not efficient
   *)
  let rec reduce_choices_aux acc lst = 
    match lst with
      | (c1, _) as e :: tl ->
          (
            let acc = 
              try
                let _ = 
                  List.find 
                    (fun (c2, _) -> c1 = c2)
                    tl
                in
                  acc
              with Not_found ->
                e :: acc
            in
              reduce_choices_aux acc tl
          )
      | [] ->
          List.rev acc
  in
    reduce_choices_aux 
      []
      (List.map (fun (cond, vl) -> reduce cond, vl) choices)
;;

