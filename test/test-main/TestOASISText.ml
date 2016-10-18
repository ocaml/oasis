(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2016, Sylvain Le Gall                                   *)
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

open OUnit2
open OASISText


let printer_oasis_text t =
  String.concat ", "
    (List.map
       (function
          | Para str -> Printf.sprintf "Para %S" str
          | Verbatim str -> Printf.sprintf "Verbatim %S" str
          | BlankLine -> "BlankLine")
       t)


let check_roundtrip ~input ~want ?input_roundtrip text_ctxt =
   let text = OASISText.of_string input in
   let output = OASISText.to_string text in
   let input_roundtrip =
     match input_roundtrip with
       | Some str -> str
       | None -> input
   in
     assert_equal
       ~printer:printer_oasis_text
       want text;
     assert_equal
       ~printer:(Printf.sprintf "%S")
       input_roundtrip output


let tests =
  "OASISText" >:::
  [
    "empty" >::
    check_roundtrip ~input:"" ~want:[];

    "blank line" >::
    check_roundtrip ~input:"\n" ~want:[BlankLine];

    "para" >::
    check_roundtrip
      ~input:"abcd\nefgh"
      ~want:[Para "abcd efgh"]
      ~input_roundtrip:"abcd efgh";

    "para blank" >::
    check_roundtrip
      ~input:"abcd\nefgh\n"
      ~want:[Para "abcd efgh"; BlankLine]
      ~input_roundtrip:"abcd efgh\n";

    "2 para" >::
    check_roundtrip
      ~input:"abcd\n\
              efgh\n\
              \n\
              ijkl"
      ~want:[Para "abcd efgh"; Para "ijkl"]
      ~input_roundtrip:"abcd efgh\n\nijkl";

    "verbatim" >::
    check_roundtrip
      ~input:"abcd\n\
              efgh\n\
              \n\
              \ ijkl"
      ~want:[Para "abcd efgh"; BlankLine; Verbatim "ijkl"]
      ~input_roundtrip:"abcd efgh\n\n ijkl";
  ]
