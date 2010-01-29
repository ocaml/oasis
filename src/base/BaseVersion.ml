
(** Version comparisons
    @author Sylvain Le Gall
  *)

type t = string
;;

type comparator = 
  | VGreater of t
  | VEqual of t
  | VLesser of t
  | VOr of  comparator * comparator
  | VAnd of comparator * comparator
;;

(** Compare versions
  *)
let version_compare v1 v2 =
  let is_digit c =
    '0' <= c && c <= '9'
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

(* END EXPORT *)

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

(** Convert a comparator to string 
  *)
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

open ODN;;

(** Convert a comparator to its code representation for inclusion
  *)
let rec code_of_comparator cmp =
  let variant vrt args =
    VRT (("BaseVersion."^vrt), args)
  in
    match cmp with
      | VGreater v  -> variant "VGreater" [STR v]
      | VEqual v    -> variant "VEqual" [STR v]
      | VLesser v   -> variant "VLesser" [STR v]
      | VOr (c1, c2)  -> 
          variant "VOr" 
            [code_of_comparator c1; 
             code_of_comparator c2]
      | VAnd (c1, c2) -> 
          variant "VAnd" 
            [code_of_comparator c1; 
             code_of_comparator c2]
;;

(** Convert a version to a varname 
  *)
let varname_of_version v =
  let buff = 
    Buffer.create (String.length v)
  in
    String.iter 
      (fun c ->
         let code = 
           Char.code
         in
         let code_c =
           code c
         in
           if (code 'a' <= code_c && code_c <= code 'z') ||
              (code 'A' <= code_c && code_c <= code 'Z') ||
              (code '0' <= code_c && code_c <= code '9') ||
              (c = '_') then
             Buffer.add_char buff c
           else
             Buffer.add_char buff '_')
      v;
    Buffer.contents buff 
;;

(** Convert a comparator to a varname 
  *)
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
