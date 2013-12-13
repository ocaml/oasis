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


(** Tests for BaseVersion
    @author Sylvain Le Gall
  *)


open OUnit2
open TestCommon
open OASISVersion


let tests =

  let vstr_compare v1 v2 =
    version_compare
      (version_of_string v1)
      (version_of_string v2)
  in

  let version_compare_of_vector (v1, v2, exp) =
    (Printf.sprintf "version_compare %S %S" v1 v2) >::
    (fun test_ctxt ->
       let norm_sign i =
         if i = 0 then
           0
         else if i < 0 then
           -1
         else
           1
       in
         assert_equal
           ~msg:(Printf.sprintf
                   "Result of '%s' and '%s' comparison"
                   v1
                   v2)
           ~printer:string_of_int
           exp
           (norm_sign (vstr_compare v1 v2)))
  in

  let comparator_apply_of_vector (v, c, exp) =
    (Printf.sprintf "comparator_apply %S %S" v c) >::
    (fun test_ctxt ->
       let op =
         comparator_of_string c
       in
         assert_equal
           ~msg:(Printf.sprintf
                   "Result of applying comparator '%s' to '%s'"
                   c
                   v)
           ~printer:string_of_bool
           exp
           (comparator_apply
              (version_of_string v)
              op))
  in

  "Version" >:::
  [
    "compare" >:::
    (List.map version_compare_of_vector
       [
         "1.0.2", "1.0.2", 0;
         "1.0.1", "1.0.2", -1;
         "1.0.3", "1.0.2", 1;
         "0.6.0", "0.7",   -1;
         "1.2.0",     "1.2.0~rc1",    1;
         "1.2.0~rc1", "1.2.0~rc2",    -1;
         "0.1.0",     "0.2.0~alpha1", -1;
         "0.2.0",     "0.2.0~alpha1", 1;
         "2.0beta", "2.0beta", 0;
       ]);

    "comparator" >:::
    (List.map comparator_apply_of_vector
       [
         "1.0.2", ">= 1.0.2", true;
         "1.0.2", "= 1.0.2", true;
         "1.0.2", "> 1.0.2", false;
         "1.0.1", ">= 1.0.2", false;
         "1.0",   ">= 1.0 && < 2.0", true;
         "4.01.0+dev1_2012-03-31", ">= 3.12", true;
       ]);

    "sort" >::
    (fun test_ctxt ->
       let lst =
         ["0.2.0~rc2"; "0.2.0~alpha1"; "0.1.0"; "0.2.0~alpha2"; "0.2.0~beta1";
          "0.2.0"]
       in
         assert_equal
           ~printer:(String.concat "; ")
           ["0.1.0"; "0.2.0~alpha1"; "0.2.0~alpha2"; "0.2.0~beta1"; "0.2.0~rc2";
            "0.2.0"]
           (List.sort vstr_compare lst));

    "back-and-forth" >::
    (fun test_ctxt ->
       let str = ">= 1.0 && <= 2.0 || = 3.0" in
       let cmp = comparator_of_string str in
       let cmp' = comparator_of_string (string_of_comparator cmp) in
         assert_equal
           ~printer:string_of_comparator
           cmp
           cmp');
  ]
