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

open FileUtil
open OUnit2
open TestCommon
open OASISPlugin
open OASISFileTemplate
open TestFullUtils


let tests =
  "Plugin OMake" >:::
  [
    "simplelib1" >::
    (fun test_ctxt ->
       let dn =
         in_testdata_dir test_ctxt ["TestOMake"; "simplelib"]
       in
       let fn = Filename.concat dn OASISParse.default_oasis_fn in
       let pkg = OASISParse.from_file ~ctxt:oasis_ctxt fn in
       let ctxt, _ =
         with_bracket_chdir test_ctxt dn
           (fun test_ctxt ->
              BaseSetup.of_package ~setup_update:false OASISSetupUpdate.NoUpdate pkg)
       in
       let () =
         assert_bool "No error during generation." (not ctxt.error)
       in
       ()
    );

    "simplelib2" >::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt
           ~is_native:(is_native test_ctxt)
           ~native_dynlink:(native_dynlink test_ctxt)
           (in_testdata_dir test_ctxt ["TestOMake"; "simplelib"])
       in
       oasis_setup test_ctxt t;
       run_ocaml_setup_ml ~check_output:true test_ctxt t
         ["-configure"];
       run_ocaml_setup_ml ~check_output:true test_ctxt t
         ["-build"];
       run_ocaml_setup_ml ~check_output:true test_ctxt t
         ["-doc"];
    );

    "complex1" >::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt
           ~is_native:(is_native test_ctxt)
           ~native_dynlink:(native_dynlink test_ctxt)
           (in_testdata_dir test_ctxt ["TestOMake"; "complex"])
       in
       oasis_setup test_ctxt t;
       run_ocaml_setup_ml ~check_output:true test_ctxt t
         ["-configure"];
       run_ocaml_setup_ml ~check_output:true test_ctxt t
         ["-build"];
       run_ocaml_setup_ml ~check_output:true test_ctxt t
         ["-doc"];
    );
  ]
