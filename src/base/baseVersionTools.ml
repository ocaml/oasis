
(** Version comparisons tools
    @author Sylvain Le Gall
  *)

open BaseVersion;;

(** Parse a comparator string 
  *)
let comparator_of_string str =
  let blank =
    function 
      | ' ' | '\t' | '\n' | '\r' -> true
      | _ -> false
  in

  (* Split a string into word *)
  let split_blank str =
    let strlen =
      String.length str
    in
    let buff =
      Buffer.create (((String.length str)/ 2) + 1)
    in
    let rec skip_blank i =
      if i < strlen then
        (
          if blank str.[i] then
            skip_blank (i + 1)
          else
            i
        )
      else
        i
    in
    let buff_dump acc =
      let nstr =
        Buffer.contents buff
      in
      let () = 
        Buffer.clear buff
      in
        if nstr = "" then
          acc
        else
          nstr :: acc
    in
    let rec split_aux acc i = 
      if i < strlen then
        (
          if blank str.[i] then
            ( 
              split_aux 
                (buff_dump acc)
                (skip_blank (i + 1))
            )
          else
            (
              Buffer.add_char buff str.[i];
              split_aux acc (i + 1)
            )
        )
      else
        (
          List.rev (buff_dump acc)
        )
    in
      split_aux [] 0
  in
    match split_blank str with 
      | [">";  v] -> VGreater v
      | [">="; v] -> VOr (VGreater v, VEqual v)
      | ["<";  v] -> VLesser v
      | ["<="; v] -> VOr (VLesser v, VEqual v)
      | ["="; v]  -> VEqual v
      | _ -> failwith ("Unrecognized comparator: "^str)
;;

(* Convert a comparator to string *)
let rec string_of_comparator =
  function 
    | VGreater v  -> "> "^v
    | VEqual v    -> "= "^v
    | VLesser v   -> "< "^v
    | VOr (VGreater v1, VEqual v2) 
    | VOr (VEqual v1, VGreater v2) when v1 = v2 ->
        ">= "^v1
    | VOr (VLesser v1, VEqual v2) 
    | VOr (VEqual v1, VLesser v2) when v1 = v2 ->
        "<= "^v1
    | VOr (c1, c2)  -> 
        (string_of_comparator c1)^" || "^(string_of_comparator c2)
    | VAnd (c1, c2) -> 
        (string_of_comparator c1)^" && "^(string_of_comparator c2)
;;

(* Convert a version to a varname *)
let varname_of_version v =
  v
;;

(* Convert a compartor to a varname *)
let rec varname_of_comparator =
  function 
    | VGreater v -> "gt_"^(varname_of_version v)
    | VLesser v -> "lt_"^(varname_of_version v)
    | VEqual v -> "eq_"^(varname_of_version v)
    | VOr (VGreater v1, VEqual v2) 
    | VOr (VEqual v1, VGreater v2) when v1 = v2 ->
        "ge_"^(varname_of_version v1)
    | VOr (VLesser v1, VEqual v2) 
    | VOr (VEqual v1, VLesser v2) when v1 = v2 ->
        "le_"^(varname_of_version v1)
    | VOr (c1, c2) ->
        (varname_of_comparator c1)^"_or_"^(varname_of_comparator c2)
    | VAnd (c1, c2) ->
        (varname_of_comparator c1)^"_and_"^(varname_of_comparator c2)
;;


