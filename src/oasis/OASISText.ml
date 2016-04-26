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

TYPE_CONV_PATH "OASISText"

type elt =
  | Para of string
  | Verbatim of string
  | BlankLine
with odn

type t = elt list with odn

(* END EXPORT *)

open Format
open FormatExt
open OASISValues


let of_string str =
  let lst = OASISString.split_newline ~do_trim:false str in
  let is_verbatim = OASISString.starts_with ~what:" " in
  let rec parse =
    function
      (* End of string special cases. *)
      | [""; ""] ->
        [`BlankLine]
      | [str; ""] ->
        if is_verbatim str then
          [`Verbatim str; `BlankLine]
        else
          [`ContPara str; `BlankLine]

      | "" :: "" :: tl ->
        `BlankLine :: (parse tl)

      | str1 :: "" :: str2 :: tl when is_verbatim str2 ->
        `ContPara str1 :: `BlankLine :: `Verbatim str2 :: (parse tl)

      | str :: "" :: tl ->
        if is_verbatim str then
          (`Verbatim str) :: `BlankLine :: (parse tl)
        else
          (`ContPara str) :: `EndPara :: (parse tl)
      | str :: tl ->
        if is_verbatim str then
          (`Verbatim str) :: (parse tl)
        else
          (`ContPara str) :: (parse tl)
      | [] ->
        []
  in
  let rec join_para =
    function
      | `ContPara str1 :: `ContPara str2 :: tl ->
        join_para ((`ContPara (str1 ^ " " ^ str2)) :: tl)
      | `EndPara :: tl ->
        join_para tl
      | (`Verbatim _ | `ContPara _ | `BlankLine) as e :: tl ->
        e :: join_para tl
      | [] ->
        []
  in
  List.map
    (function
      | `ContPara str -> Para str
      | `Verbatim str ->
        Verbatim (String.sub str 1 ((String.length str) - 1))
      | `BlankLine -> BlankLine)
    (join_para (parse lst))


let pp_print_verbatim fmt str =
  pp_print_char fmt ' ';
  pp_print_string fmt str


let rec pp_print fmt =
  function
    | [Para str] ->
      pp_print_para fmt ~end_para:false str
    | [Verbatim str] ->
      pp_print_verbatim fmt str
    | [Para str; BlankLine] ->
      pp_print_para fmt ~end_para:false str;
      pp_print_newline fmt ()
    | [Verbatim str; BlankLine] ->
      pp_print_verbatim fmt str;
      pp_print_newline fmt ()
    | Para str :: BlankLine :: ((Verbatim _ :: _) as tl) ->
      pp_print_para fmt str;
      pp_print fmt tl
    | Para str :: ((Verbatim _ :: _) as tl) ->
      pp_print_para ~end_para:false fmt str;
      pp_print_newline fmt ();
      pp_print fmt tl
    | Para str :: tl  ->
      pp_print_para fmt str;
      pp_print fmt tl
    | Verbatim str :: tl ->
      pp_print_verbatim fmt str;
      pp_print_newline fmt ();
      pp_print fmt tl
    | BlankLine :: tl ->
      pp_print_newline fmt ();
      pp_print fmt tl
    | [] ->
      ()


let to_string t =
  let buff = Buffer.create 13 in
  let fmt = Format. formatter_of_buffer buff in
  pp_print fmt t;
  pp_print_flush fmt ();
  Buffer.contents buff


let value =
  {
    parse  = (fun ~ctxt s -> of_string s);
    update = update_fail;
    print  = to_string;
  }
