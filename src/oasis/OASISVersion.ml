
(** Version comparisons
    @author Sylvain Le Gall
  *)

open OASISTypes
open CommonGettext

(** Compare versions
  *)
let rec version_compare v1 v2 =
  compare v1 v2

(** Convert a string into a version
  *)
let version_of_string str =
  let is_digit c =
    '0' <= c && c <= '9'
  in

  let str_len =
    String.length str
  in

  let buff =
    Buffer.create str_len
  in

  let rec extract_filter test start = 
    if start < str_len && test str.[start] then
      (
        Buffer.add_char buff str.[start];
        extract_filter test (start + 1)
      )
    else
      (
        let res =
          Buffer.contents buff
        in
          Buffer.clear buff;
          res, start
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

  let rec parse_aux pos =
    if pos < str_len then
      begin
        if is_digit str.[pos] then
          begin
            let vl, end_pos =
              extract_int pos
            in
              VInt (vl, parse_aux end_pos)
          end
        else
          begin
            let vl, end_pos =
              extract_non_int pos
            in
              VNonInt (vl, parse_aux end_pos)
          end
      end
    else
      VEnd 
  in

  let rec compress =
    function
      | VInt (i, VNonInt(".", (VInt _ as tl))) ->
          VInt (i, compress tl)
      | VInt (i, tl) ->
          VInt (i, compress tl)
      | VNonInt (i, tl) ->
          VNonInt (i, compress tl)
      | VEnd ->
          VEnd
  in

    compress (parse_aux 0)

(** Convert a version to a string
  *)
let rec string_of_version =
  function
    | VInt (i, (VInt _ as tl)) ->
        (string_of_int i)^"."^(string_of_version tl)
    | VInt (i, tl) -> 
        (string_of_int i)^(string_of_version tl)
    | VNonInt (s, tl) -> 
        s^(string_of_version tl)
    | VEnd -> ""

(** Apply version comparator expression
  *)
let rec comparator_apply v op =
  match op with
    | VGreater cv ->
        (version_compare v cv) > 0
    | VGreaterEqual cv ->
        (version_compare v cv) >= 0
    | VLesser cv ->
        (version_compare v cv) < 0
    | VLesserEqual cv ->
        (version_compare v cv) <= 0
    | VEqual cv ->
        (version_compare v cv) = 0
    | VOr (op1, op2) ->
        (comparator_apply v op1) || (comparator_apply v op2)
    | VAnd (op1, op2) ->
        (comparator_apply v op1) && (comparator_apply v op2)

(** Convert a comparator to string 
  *)
let rec string_of_comparator =
  function 
    | VGreater v  -> "> "^(string_of_version v)
    | VEqual v    -> "= "^(string_of_version v)
    | VLesser v   -> "< "^(string_of_version v)
    | VGreaterEqual v -> ">= "^(string_of_version v)
    | VLesserEqual v  -> "<= "^(string_of_version v)
    | VOr (c1, c2)  -> 
        (string_of_comparator c1)^" || "^(string_of_comparator c2)
    | VAnd (c1, c2) -> 
        (string_of_comparator c1)^" && "^(string_of_comparator c2)

(** Convert a version to a varname 
  *)
let varname_of_version v =
  let vstr = 
    string_of_version v
  in
  let buff = 
    Buffer.create (String.length vstr)
  in
    String.iter 
      (fun c ->
         if ('a' <= c && c <= 'z') ||
            ('A' <= c && c <= 'Z') ||
            ('0' <= c && c <= '9') ||
            (c = '_') then
           Buffer.add_char buff c
         else
           Buffer.add_char buff '_')
      vstr;
    Buffer.contents buff 

(** Convert a comparator to a varname 
  *)
let rec varname_of_comparator =
  function 
    | VGreater v -> "gt_"^(varname_of_version v)
    | VLesser v  -> "lt_"^(varname_of_version v)
    | VEqual v   -> "eq_"^(varname_of_version v)
    | VGreaterEqual v -> "ge_"^(varname_of_version v)
    | VLesserEqual v  -> "le_"^(varname_of_version v)
    | VOr (c1, c2) ->
        (varname_of_comparator c1)^"_or_"^(varname_of_comparator c2)
    | VAnd (c1, c2) ->
        (varname_of_comparator c1)^"_and_"^(varname_of_comparator c2)

(* END EXPORT *)

open OASISAstTypes

(** Convert a string into a comparator
  *)
let comparator_of_string str =
  let rec parse_aux =
    function
      | VCAnd (c1, c2) -> VAnd (parse_aux c1, parse_aux c2)
      | VCOr (c1, c2)  -> VOr (parse_aux c1, parse_aux c2)
      | VCGt s -> VGreater (version_of_string s)
      | VCGe s -> VGreaterEqual (version_of_string s)
      | VCEq s -> VEqual (version_of_string s)
      | VCLt s -> VLesser (version_of_string s)
      | VCLe s -> VLesserEqual (version_of_string s)
  in
  let lexbuf =
    Lexing.from_string str
  in
    try 
      parse_aux 
        (OASISVersion_parser.main 
           OASISVersion_lexer.token lexbuf)
    with e ->
      failwith
        (Printf.sprintf
           (f_ "Error while parsing '%s': %s")
           str
           (Printexc.to_string e))
