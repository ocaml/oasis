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
open TestFullUtils
open TestCommon


let tests =
  "Plugin Internal" >:::
  [
    "feature findlib_directory" >::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt
           ~is_native:(is_native test_ctxt)
           ~native_dynlink:(native_dynlink test_ctxt)
           (in_testdata_dir test_ctxt
              ["TestPluginInternal"; "findlib_directory"])
       in
       oasis_setup test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t oasis_ocamlbuild_files;
       register_generated_files t ["META"; "library.mldylib"; "library.mllib"];
       register_installed_files test_ctxt t
         [InstalledOCamlLibrary
            ("foobar",
             ["META";
              "lib/L.annot"; "lib/L.cmi"; "lib/L.cmt"; "lib/L.cmx"; "lib/L.ml";
              "lib/obj/O.cmi"; "lib/obj/O.cmo"; "lib/obj/O.cmx"; "lib/obj/O.ml";
              "lib/obj/O.o"; "lib/library.a"; "lib/library.cma";
              "lib/library.cmxa"; "lib/library.cmxs"])];
       (* Run standard test. *)
       standard_test test_ctxt t;
       try_installed_library test_ctxt t "foobar" ["L"];
       try_installed_library test_ctxt t "foobar.object" ["O"]);

    "feature findlib_iextra_files" >::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt
           ~is_native:(is_native test_ctxt)
           ~native_dynlink:(native_dynlink test_ctxt)
           (in_testdata_dir test_ctxt
              ["TestPluginInternal"; "findlib_extra_files"])
       in
       oasis_setup test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t oasis_ocamlbuild_files;
       register_generated_files t ["META"; "library.mldylib"; "library.mllib"];
       register_installed_files test_ctxt t
         [InstalledOCamlLibrary
            ("foobar",
             ["META";
              "L.annot"; "L.cmi"; "L.cmt"; "L.cmx"; "L.ml"; "_oasis";
              "library.a"; "library.cma"; "library.cmxa"; "library.cmxs"])];
       (* Run standard test. *)
       standard_test test_ctxt t);
  ]
