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


(** Tests for PropList
    @author Sylvain Le Gall
  *)


open OUnit2
open TestCommon
open PropList
open PropList.Field


let tests =
  "PropList" >::
  (fun test_ctxt ->
     let toto =
       Field.create
         ~default:1
         ~parse:(fun ?context s -> int_of_string s)
         ()
     in

     let data =
       Data.create ()
     in

     let assert_equal_int =
       assert_equal
         ~printer:string_of_int
     in

       (* Default *)
       assert_equal_int
         1
         (fget data toto);

       (* Assign *)
       fset data toto 2;
       assert_equal_int
         2
         (fget data toto);

       (* Parse *)
       fsets data toto "3";
       assert_equal_int
         3
         (fget data toto);

       ())
