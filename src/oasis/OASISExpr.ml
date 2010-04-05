(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2008-2010, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

(** OASIS expression manipulation
  *)

open OASISTypes
open OASISGettext
open OASISUtils

(** Evaluate each conditions and choose the right one. *)
let choose ?printer ?name var_get test_get lst =
  let rec eval =
    function
      | EBool b ->
          b

      | ENot e -> 
          not (eval e)

      | EAnd (e1, e2) ->
          (eval e1) && (eval e2)

      | EOr (e1, e2) -> 
          (eval e1) || (eval e2)

      | EFlag nm ->
          let v =
            var_get nm
          in
            assert(v = "true" || v = "false");
            (v = "true")

      | ETest (nm, vl) ->
          let v =
            test_get nm
          in
            (v = vl)
  in

  let rec choose_aux = 
    function
      | (cond, vl) :: tl ->
          if eval cond then 
            vl 
          else
            choose_aux tl
      | [] ->
          let str_lst = 
            if lst = [] then
              s_ "<empty>"
            else
              String.concat 
                (s_ ", ")
                (List.map
                   (fun (cond, vl) ->
                      match printer with
                        | Some p -> p vl
                        | None -> "<no printer>")
                   lst)
          in
            match name with 
              | Some nm ->
                  failwithf2
                    (f_ "No result for the choice list '%s': %s")
                    nm
                    str_lst
              | None ->
                  failwithf1
                    (f_ "No result for a choice list: %s")
                    str_lst
  in
    choose_aux (List.rev lst)

(** All availbable expression tests and functions to convert it
    to string and reverse
  *)
let expr_tests, string_of_expr_test, expr_test_of_string =
  let all =
    [
      TOs_type;
      TSystem;
      TArchitecture;
      TCcomp_type;
      TOCaml_version;
    ]
  in
  let to_string = 
    function 
      | TOs_type       -> "os_type"
      | TSystem        -> "system"
      | TArchitecture  -> "architecture"
      | TCcomp_type    -> "ccomp_type"
      | TOCaml_version -> "ocaml_version"
  in
  let of_string =
    let mp =
      List.rev_map (fun e -> to_string e, e) all
    in
      fun s -> 
        try 
          List.assoc (String.lowercase s) mp 
        with Not_found ->
          failwithf1 (f_ "Unknown OASIS test %s") s
  in
    all, to_string, of_string

(* END EXPORT *)

open OASISAstTypes

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
          if not (List.exists (lowercase_eq nm) ctxt.valid_flags) then
            failwithf1 (f_ "Unknown flag '%s'") nm
      | ETest (_, _) ->
          ()
  in
    check_aux ctxt 

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

