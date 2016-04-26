(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2013, Sylvain Le Gall                                   *)
(* Copyright (C) 2008-2011, OCamlCore SARL                                    *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)


TYPE_CONV_PATH "OASISExpr"


open OASISGettext


type test = string with odn


type flag = string with odn


type t =
  | EBool of bool
  | ENot of t
  | EAnd of t * t
  | EOr of t * t
  | EFlag of flag
  | ETest of test * string
with odn


type 'a choices = (t * 'a) list with odn


let eval var_get t =
  let rec eval' =
    function
      | EBool b ->
        b

      | ENot e ->
        not (eval' e)

      | EAnd (e1, e2) ->
        (eval' e1) && (eval' e2)

      | EOr (e1, e2) ->
        (eval' e1) || (eval' e2)

      | EFlag nm ->
        let v =
          var_get nm
        in
        assert(v = "true" || v = "false");
        (v = "true")

      | ETest (nm, vl) ->
        let v =
          var_get nm
        in
        (v = vl)
  in
  eval' t


let choose ?printer ?name var_get lst =
  let rec choose_aux =
    function
      | (cond, vl) :: tl ->
        if eval var_get cond then
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
                      | None -> s_ "<no printer>")
                 lst)
        in
        match name with
          | Some nm ->
            failwith
              (Printf.sprintf
                 (f_ "No result for the choice list '%s': %s")
                 nm str_lst)
          | None ->
            failwith
              (Printf.sprintf
                 (f_ "No result for a choice list: %s")
                 str_lst)
  in
  choose_aux (List.rev lst)


(* END EXPORT *)


open OASISUtils


let tests =
  [
    "os_type";
    "system";
    "architecture";
    "ccomp_type";
    "ocaml_version";
  ]


let test_of_string str =
  (* TODO: check for correct syntax of str *)
  str


let string_of_test t =
  t


let check valid_flags =
  let lowercase_eq str1 str2 =
    (String.lowercase str1) = (String.lowercase str2)
  in

  let rec check_aux valid_flags =
    function
      | EBool _ ->
        ()
      | ENot e ->
        check_aux valid_flags e
      | EAnd (e1, e2) | EOr (e1, e2) ->
        check_aux valid_flags e1;
        check_aux valid_flags e2
      | EFlag nm ->
        if not (List.exists (lowercase_eq nm) valid_flags) then
          failwithf (f_ "Unknown flag '%s'") nm
      | ETest (_, _) ->
        ()
  in
  check_aux valid_flags


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
    | (ENot _ | EAnd (_, _) | EOr (_, _) | EFlag _ | ETest (_, _)
      | (EBool _)) as e ->
      e


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


let if_then_else t choices_if choices_else =
  let choices_if' =
    List.rev_map (fun (t', v) -> EAnd (t, t'), v) choices_if
  in
  let choices_else' =
    List.rev_map (fun (t', v) -> EAnd (ENot t, t'), v) choices_else
  in
  reduce_choices (List.rev_append choices_else' (List.rev choices_if'))


let rec to_string =
  function
    | EBool b -> string_of_bool b
    | EFlag nm -> "flag("^nm^")"
    | ETest (nm, vl) -> nm^"("^vl^")"

    | EOr (e1, e2) ->
      (to_string e1)^" || "^(to_string e2)

    | ENot (EBool _ | EFlag _ | ETest _ | ENot _ as e) ->
      "!"^(to_string e)
    | ENot (EAnd _ | EOr _ as e) ->
      "!("^(to_string e)^")"

    | EAnd ((EOr _ as e1), (EOr _ as e2)) ->
      "("^(to_string e1)^") && ("^(to_string e2)^")"
    | EAnd ((EOr _ as e1), e2) ->
      "("^(to_string e1)^") && "^(to_string e2)
    | EAnd (e1, (EOr _ as e2)) ->
      (to_string e1)^" && ("^(to_string e2)^")"
    | EAnd (e1, e2) ->
      (to_string e1)^" && "^(to_string e2)


let string_of_choices f lst =
  "["^(String.concat "; "
      (List.rev_map (fun (e, vl) -> (to_string e)^" -> "^(f vl)) lst))^"]"
