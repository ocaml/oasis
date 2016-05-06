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


type s = string


type t = string


type comparator =
  | VGreater of t
  | VGreaterEqual of t
  | VEqual of t
  | VLesser of t
  | VLesserEqual of t
  | VOr of  comparator * comparator
  | VAnd of comparator * comparator

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

(* The comparator_reduce function transforms the given comparator into its
 * disjunctive normal form considering, with all version in ascending order. It
 * uses intervals of version to combine the terms of the comparator.
*)
let comparator_reduce =
  (* Compare endpoints *)
  let cmp_norm e1 e2 =
    match e1, e2 with
      | `BeforeFirst, `BeforeFirst | `AfterLast, `AfterLast -> `EQ
      | `BeforeFirst, _ | _, `AfterLast -> `AB
      | _, `BeforeFirst | `AfterLast, _ -> `BA
      | `Version v1, `Version v2 ->
        let d = version_compare v1 v2 in
        if d = 0 then `EQ else if d < 0 then `AB else `BA
  in
  let split e1 e2 e3 tl =
    match e2 with
      | `Version v2 -> `Interval(e1, e2) :: `Point v2 :: `Interval(e2, e3) :: tl
      | _ -> assert false
  in
  let pushif op acc b1 b2 e = if op b1 b2 then e :: acc else acc in
  (* Combine heads of intervals and continue processing. *)
  let rec combine op acc hd1 tl1 hd2 tl2 =
    let id, cons, pacc = (fun i -> i), (fun i j -> i :: j), pushif op acc in
    let m ?(acc=acc) ?(f1=id) ?(f2=id) () = merge op acc (f1 tl1) (f2 tl2) in
    match hd1, hd2 with
      | `Interval(e1, e2), `Interval (e3, e4) ->
        begin
          match cmp_norm e3 e1, cmp_norm e1 e4, cmp_norm e2 e4 with
            | `BA,   _, _   -> combine op acc hd2 tl2 hd1 tl1
            | `EQ,   _, `EQ -> m ~acc:(pacc true true hd1) ()
            | `AB, `EQ,   _ -> m ~acc:(pacc false true hd2) ~f1:(cons hd1) ()
            | `AB, `BA,   _ -> m ~acc:(pacc false true hd2) ~f1:(cons hd1) ()
            | `AB, `AB, `BA -> m ~f1:(split e1 e4 e2) ~f2:(split e3 e1 e4) ()
            | `AB, `AB, `EQ -> m ~f1:(cons hd1) ~f2:(split e3 e1 e4) ()
            | `AB, `AB, `AB -> m ~f1:(cons hd1) ~f2:(split e3 e1 e4) ()
            | `EQ,   _, `BA -> m ~f1:(split e1 e4 e2) ~f2:(cons hd2) ()
            | `EQ,   _, `AB -> m ~f1:(cons hd1) ~f2:(split e3 e2 e4) ()
        end
      | `Interval(e1, e2), `Point v3 ->
        begin
          let e3 = `Version v3 in
          match cmp_norm e3 e1, cmp_norm e2 e3 with
            | `BA, `BA -> m ~f1:(split e1 e3 e2) ~f2:(cons hd2) ()
            | (`EQ | `AB), _ -> m ~acc:(pacc false true hd2) ~f1:(cons hd1) ()
            | _, (`EQ | `AB) -> m ~acc:(pacc true false hd1) ~f2:(cons hd2) ()
        end
      | `Point v1, `Point v2 ->
        begin
          match cmp_norm (`Version v1) (`Version v2) with
            | `EQ -> m ~acc:(pacc true true hd1) ()
            | `AB -> m ~acc:(pacc true false hd1) ~f2:(cons hd2) ()
            | `BA -> m ~acc:(pacc false true hd2) ~f1:(cons hd1) ()
        end
      | `Point _, `Interval _ -> combine op acc hd2 tl2 hd1 tl1
  (* Reduce a list of segment when we can find some patterns. *)
  and reduce acc i =
    match i with
      | `Interval(e1, `Version v2) :: `Point v3
        :: `Interval(`Version v4, e5) :: tl when v2 = v3 && v3 = v4 ->
        reduce [] (List.rev_append acc (`Interval(e1, e5) :: tl))
      | hd :: tl -> reduce (hd :: acc) tl
      | [] -> List.rev acc
  and merge op acc i1 i2 =
    match i1, i2 with
      | hd1 :: tl1, hd2 :: tl2 -> combine op acc hd1 tl1 hd2 tl2
      | hd :: tl, [] -> merge op (pushif op acc true false hd) tl []
      | [], hd :: tl -> merge op (pushif op acc false true hd) [] tl
      | [], [] -> reduce [] (List.rev acc)
  in
  let rec of_comparator =
    function
      | VGreater v -> [`Interval(`Version v, `AfterLast)]
      | VLesser v  -> [`Interval(`BeforeFirst, `Version v)]
      | VEqual v   -> [`Point v]
      | VGreaterEqual v -> [`Point v; `Interval(`Version v, `AfterLast)]
      | VLesserEqual v  -> [`Interval(`BeforeFirst, `Version v); `Point v]
      | VOr (c1, c2)  -> merge ( || ) [] (of_comparator c1) (of_comparator c2)
      | VAnd (c1, c2) -> merge ( && ) [] (of_comparator c1) (of_comparator c2)
  in
  let to_comparator i =
    let cmp_true = VOr(VLesserEqual "0", VGreaterEqual "0") in
    let rec close_interval acc i c e =
      match e, i with
        | `Version v1, `Point v2 :: tl when v1 = v2 ->
          combine_intervals acc tl c (VLesserEqual v1)
        | `Version v, _ -> combine_intervals acc i c (VLesser v)
        | `AfterLast, _ -> combine_intervals acc i c cmp_true
        | `BeforeFirst, _ -> assert false
    and combine_intervals acc i c1 c2 =
      let vor c1 c2 = if c1 = cmp_true then c2 else VOr(c1, c2) in
      match c1 = cmp_true, c2 = cmp_true with
        | true, true -> map_intervals cmp_true i
        | true, false -> map_intervals (vor acc c2) i
        | false, true -> map_intervals (vor acc c1) i
        | false, false -> map_intervals (vor acc (VAnd(c1, c2))) i
    and map_intervals acc i =
      match i with
        | `Point v1 :: `Interval(`Version v2, e3) :: tl when v1 = v2 ->
          close_interval acc tl (VGreaterEqual v2) e3
        | `Interval(`BeforeFirst, e) :: tl -> close_interval acc tl cmp_true e
        | `Interval(`Version v, e) :: tl -> close_interval acc tl (VGreater v) e
        | `Interval(`AfterLast, _) :: _  -> assert false
        | `Point v :: tl -> combine_intervals acc tl (VEqual v) cmp_true
        | [] -> assert (acc <> cmp_true); acc
    in
    map_intervals cmp_true i
  in
  fun v -> to_comparator (of_comparator v)


open OASISValues


let value =
  {
    parse  = (fun ~ctxt:_ s -> version_of_string s);
    update = update_fail;
    print  = string_of_version;
  }


let comparator_value =
  {
    parse  = (fun ~ctxt:_ s -> comparator_of_string s);
    update = update_fail;
    print  = string_of_comparator;
  }
