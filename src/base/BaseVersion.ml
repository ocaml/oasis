
(** Version comparisons
    @author Sylvain Le Gall
  *)

type version = string
;;

type version_comparator = 
  | VGreater of version
  | VEqual of version
  | VLesser of version
  | VOr of  version_comparator * version_comparator
  | VAnd of version_comparator * version_comparator
;;

(** Compare versions
  *)
let version_compare v1 v2 =
  let is_digit =
    function
      | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
      | _ -> false
  in

  let buff =
    Buffer.create (String.length v1)
  in

  let rec extract_filter test (v, start, len) = 
    if start < len && test v.[start] then
      (
        Buffer.add_char buff v.[start];
        extract_filter test (v, start + 1, len)
      )
    else
      (
        let res =
          Buffer.contents buff
        in
          Buffer.clear buff;
          res, (v, start, len)
      )
  in
  let extract_int vpos =
    let str, vpos =
      extract_filter is_digit vpos
    in
      int_of_string str, vpos
  in
  let extract_non_int vpos =
    extract_filter 
      (fun c -> not (is_digit c)) 
      vpos
  in
  let rec compare_aux ((v1,start1,len1) as vpos1) ((v2,start2,len2) as vpos2) = 
    if start1 < len1 && start2 < len2 then
      (
        if is_digit v1.[start1] && is_digit v2.[start2] then
          (
            let i1, vpos1 =
              extract_int vpos1
            in
            let i2, vpos2 =
              extract_int vpos2
            in
              match i1 - i2 with
                | 0 -> compare_aux vpos1 vpos2
                | n -> n
          )
        else
          (
            let str1, vpos1 =
              extract_non_int vpos1
            in
            let str2, vpos2 =
              extract_non_int vpos2
            in
              match String.compare str1 str2 with
                | 0 -> compare_aux vpos1 vpos2
                | n -> n
          )
      )
    else 
      (
        len1 - len2 
      )
  in
    compare_aux 
      (v1, 0, (String.length v1))
      (v2, 0, (String.length v2))
;;

(** Apply version comparator expression
  *)
let rec comparator_apply v op =
  match op with
    | VGreater cversion ->
        (version_compare v cversion) > 0
    | VLesser cversion ->
        (version_compare v cversion) < 0
    | VEqual cversion ->
        (version_compare v cversion) = 0
    | VOr (op1, op2) ->
        (comparator_apply v op1) || (comparator_apply v op2)
    | VAnd (op1, op2) ->
        (comparator_apply v op1) && (comparator_apply v op2)
;;
