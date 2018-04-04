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
open OASISValues
open TestCommon

let tests =
  let expect_failure v s test_ctxt=
    try
      let ctxt = oasis_ctxt test_ctxt in
      let _ = v.parse ~ctxt s in
      assert_failure "expected an exception"
    with Failure _ ->
      ()
  in

  "OASISValues" >:::
  [
    "findlib_full_invalid_empty" >::
    expect_failure findlib_full "";

    "comma_separated_findlib_full_with_one_invalid_empty" >::
    expect_failure
      (comma_separated
         (with_optional_parentheses
            findlib_full
            OASISVersion.comparator_value))
      "pkg1, pkg2,";
  ]
