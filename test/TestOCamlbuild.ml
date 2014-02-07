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

open OUnit2
open TestCommon
open OASISPlugin
open OASISFileTemplate


let tests =
  "Plugin OCamlbuild" >::
  (fun test_ctxt ->
     let dn = in_testdata_dir test_ctxt ["TestOCamlbuild"; "missing-source"] in
     let fn = Filename.concat dn "_oasis" in
     let pkg = OASISParse.from_file ~ctxt:oasis_ctxt fn in
     let ctxt, _ =
       with_bracket_chdir test_ctxt dn
         (fun test_ctxt ->
            BaseSetup.of_package ~setup_update:false pkg)
     in
     let () =
       assert_bool "No error during generation." (not ctxt.error)
     in
     let tmpl = find "test.mllib" ctxt.files in
       match tmpl.body with
         | Body lst | BodyWithDigest (_, lst) ->
             assert_equal
               ~printer:(fun lst ->
                           String.concat ", "
                             (List.map (Printf.sprintf "%S") lst))
               ["A"; "B"; "C"]
               (List.sort String.compare lst);
         | NoBody ->
             assert_failure "No content for test.mllib.")
