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


open OASISGettext


TYPE_CONV_PATH "OASISVersion"


type s = string


type t = string with odn


type comparator =
  | VGreater of t
  | VGreaterEqual of t
  | VEqual of t
  | VLesser of t
  | VLesserEqual of t
  | VOr of  comparator * comparator
  | VAnd of comparator * comparator
  with odn


(* Range of allowed characters *)
let is_digit c =
  '0' <= c && c <= '9'


let is_alpha c =
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')


let is_special =
  function
    | '.' | '+' | '-' | '~' -> true
    | _ -> false


let rec version_compare v1 v2 =
  if v1 <> "" || v2 <> "" then
    begin
      (* Compare ascii string, using special meaning for version
       * related char
       *)
      let val_ascii c =
        if c = '~' then -1
        else if is_digit c then 0
        else if c = '\000' then 0
        else if is_alpha c then Char.code c
        else (Char.code c) + 256
      in

      let len1 = String.length v1 in
      let len2 = String.length v2 in

      let p = ref 0 in

      (** Compare ascii part *)
      let compare_vascii () =
        let cmp = ref 0 in
        while !cmp = 0 &&
              !p < len1 && !p < len2 &&
              not (is_digit v1.[!p] && is_digit v2.[!p]) do
          cmp := (val_ascii v1.[!p]) - (val_ascii v2.[!p]);
          incr p
        done;
        if !cmp = 0 && !p < len1 && !p = len2 then
          val_ascii v1.[!p]
        else if !cmp = 0 && !p = len1 && !p < len2 then
          - (val_ascii v2.[!p])
        else
          !cmp
      in

      (** Compare digit part *)
      let compare_digit () =
        let extract_int v p =
          let start_p = !p in
            while !p < String.length v && is_digit v.[!p] do
              incr p
            done;
            let substr =
              String.sub v !p ((String.length v) - !p)
            in
            let res =
              match String.sub v start_p (!p - start_p) with
                | "" -> 0
                | s -> int_of_string s
            in
              res, substr
        in
        let i1, tl1 = extract_int v1 (ref !p) in
        let i2, tl2 = extract_int v2 (ref !p) in
          i1 - i2, tl1, tl2
      in

        match compare_vascii () with
          | 0 ->
              begin
                match compare_digit () with
                  | 0, tl1, tl2 ->
                      if tl1 <> "" && is_digit tl1.[0] then
                        1
                      else if tl2 <> "" && is_digit tl2.[0] then
                        -1
                      else
                        version_compare tl1 tl2
                  | n, _, _ ->
                      n
              end
          | n ->
              n
    end
  else
    begin
      0
    end


let version_of_string str = str


let string_of_version t = t


let version_compare_string s1 s2 =
  version_compare (version_of_string s1) (version_of_string s2)


let chop t =
  try
    let pos =
      String.rindex t '.'
    in
      String.sub t 0 pos
  with Not_found ->
    t


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


let rec varname_of_comparator =
  let concat p v =
    OASISUtils.varname_concat
      p
      (OASISUtils.varname_of_string
         (string_of_version v))
  in
    function
      | VGreater v -> concat "gt" v
      | VLesser v  -> concat "lt" v
      | VEqual v   -> concat "eq" v
      | VGreaterEqual v -> concat "ge" v
      | VLesserEqual v  -> concat "le" v
      | VOr (c1, c2) ->
          (varname_of_comparator c1)^"_or_"^(varname_of_comparator c2)
      | VAnd (c1, c2) ->
          (varname_of_comparator c1)^"_and_"^(varname_of_comparator c2)


let rec comparator_ge v' =
  let cmp v = version_compare v v' >= 0 in
  function
    | VEqual v
    | VGreaterEqual v
    | VGreater v -> cmp v
    | VLesserEqual _
    | VLesser _ -> false
    | VOr (c1, c2) -> comparator_ge v' c1 || comparator_ge v' c2
    | VAnd (c1, c2) -> comparator_ge v' c1 && comparator_ge v' c2


(* END EXPORT *)


open OASISUtils
open OASISVersion_types


let comparator_of_string str =
  let lexbuf =
    Lexing.from_string str
  in
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
    try
      parse_aux
        (OASISVersion_parser.main
           OASISVersion_lexer.token lexbuf)
    with e ->
      failwithf
        (f_ "Error while parsing '%s': %s")
        str
        (Printexc.to_string e)


let rec comparator_reduce =
  function
    | VAnd (v1, v2) ->
        (* TODO: this can be improved to reduce more *)
        let v1 =
          comparator_reduce v1
        in
        let v2 =
          comparator_reduce v2
        in
          if v1 = v2 then
            v1
          else
            VAnd (v1, v2)
    | cmp ->
        cmp


open OASISValues


let value =
  {
    parse  = (fun ~ctxt s -> version_of_string s);
    update = update_fail;
    print  = string_of_version;
  }


let comparator_value =
  {
    parse  = (fun ~ctxt s -> comparator_of_string s);
    update = update_fail;
    print  = string_of_comparator;
  }
