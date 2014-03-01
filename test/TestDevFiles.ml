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


(** Test the devfiles plugin

    @author Sylvain Le Gall
  *)


open TestCommon
open OUnit2
open TestFullUtils

let gen_tests ~is_native =
  let setup_test_directories test_ctxt fpath path =
    setup_test_directories test_ctxt ~is_native
      ~native_dynlink:(native_dynlink test_ctxt)
      (fpath test_ctxt path)
  in
  [ "DevFiles" >::
    (fun test_ctxt ->
       let tmpdir = bracket_tmpdir test_ctxt in
       FileUtil.cp
         [in_testdata_dir test_ctxt ["TestDevFiles"; "test-devfiles1.oasis"]]
         (Filename.concat tmpdir "_oasis");
       assert_oasis_cli ~ctxt:test_ctxt ["-C"; tmpdir; "setup"];
       if Sys.os_type <> "Win32" then
         assert_command ~ctxt:test_ctxt ~chdir:tmpdir
           "./configure" ["--prefix=/usr"; "--mandir=/usr/share/man";
                          "--infodir=/usr/share/info"; "--datadir=/usr/share";
                          "--sysconfdir=/etc"; "--localstatedir=/var/lib"];
       ());

    "dyncomp">::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt in_testdata_dir
           ["TestDevFiles"; "dyncomp"]
       in
       oasis_setup ~dynamic:true test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t oasis_ocamlbuild_files;
       register_generated_files t ["configure"; "Makefile"; "setup.exe"];
       (* Run standard test. *)
       standard_test_compiled test_ctxt t;
       let makefile_content = file_content (t.src_dir ^ "/Makefile") in
       assert_bool
         "Test the SETUP variable in the Makefile for dyncomp."
         (OASISString.contains ~what:"SETUP = ./setup.exe" makefile_content)
    );
  ]

let tests =
  "TestDevFiles" >:::
  [
    "best=native" >:::
    (skip_test_on_non_native_arch
       (gen_tests ~is_native:true));

    "best=byte" >:::
    (gen_tests ~is_native:false);
  ]
