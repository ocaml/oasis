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


(** Run full OASIS use cases
    @author Sylvain Le Gall
  *)


open FileUtil
open OUnit2
open TestCommon
open TestFullUtils


let gen_tests ~is_native () =
  let native_dynlink test_ctxt =
    if is_native then
      native_dynlink test_ctxt
    else
      false
  in
  let setup_test_directories test_ctxt fpath path =
    setup_test_directories test_ctxt ~is_native
      ~native_dynlink:(native_dynlink test_ctxt)
      (fpath test_ctxt path)
  in
  [
   (* TODO: reactivate *)
(*
    (* Use flags *)
    "examples/flags" >::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt in_example_dir ["flags"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            [
              "src/simplelib/simplelib.mllib";
              "src/simplelib/simplelib.mldylib";
              "src/simplelib/simplelib.odocl";
              "src/simplelibext/simplelibext.mllib";
              "src/simplelibext/simplelibext.mldylib";
              "src/simplelibext/simplelibext.odocl";
            ]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("simplelib",
                ["simplelib.cma";
                 "Foo.cmi"; "Foo.ml"; "Foo.annot"; "Foo.cmti"; "Foo.cmt";
                 "Bar.cmi"; "Bar.ml"; "Bar.annot"; "Bar.cmti"; "Bar.cmt";
                 "META";
                 "simplelib.cmxa";
                 "simplelib.cmxs";
                 "Foo.cmx"; "Bar.cmx";
                 "simplelib.a"]);
             InstalledOCamlLibrary
               ("simplelibext",
                ["simplelibext.cma";
                 "FooExt.cmi"; "FooExt.ml"; "FooExt.annot"; "FooExt.cmti";
                 "FooExt.cmt";
                 "BarExt.cmi"; "BarExt.ml"; "BarExt.annot"; "BarExt.cmti";
                 "BarExt.cmt";
                 "META";
                 "simplelibext.cmxa";
                 "simplelibext.cmxs";
                 "FooExt.cmx"; "BarExt.cmx";
                 "simplelibext.a"]);
             InstalledAPIRef("simplelib", ["Foo"; "Bar"]);
             InstalledAPIRef("simplelibext", ["FooExt"; "BarExt"]);
           ];
         (* Run standard test. *)
         standard_test test_ctxt t);
 *)

   (* TODO: reactivate *)
(*
    (* Complete library *)
    "examples/simplelib" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_example_dir ["simplelib"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            [
              "src/simplelib.mllib";
              "src/simplelib.mldylib";
              "src/simplelib.odocl";
            ]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("simplelib" ,
                ["simplelib.cma";
                 "foo.cmi"; "foo.mli"; "foo.annot"; "foo.cmt"; "foo.cmti";
                 "bar.cmi"; "bar.mli"; "bar.annot"; "bar.cmt"; "bar.cmti";
                 "META";
                 "simplelib.cmxa";
                 "simplelib.cmxs";
                 "foo.cmx"; "bar.cmx";
                 "simplelib.a"]);
             InstalledAPIRef("simplelib", ["Bar"; "Foo"]);
           ];
         (* Run standard test. *)
         standard_test test_ctxt t);
 *)

    (* Packed library *)
    "examples/packedlib" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_example_dir ["packedlib"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            ["src/packedlib.mlpack";
             "src/packedlib.mllib";
             "src/packedlib.mldylib";
             "src/META"]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("packedlib",
                ["packedlib.cma"; "packedlib.cmi"; "packedlib.cmx";
                 "foo.mli"; "bar.mli"; "Baz.ml"; "META";
                 "packedlib.cmxa"; "packedlib.cmxs";
                 "packedlib.a";
                 "Baz.annot"; "Baz.cmt"; "bar.annot"; "bar.cmt"; "bar.cmti";
                 "foo.annot"; "foo.cmt"; "foo.cmti";
                 "packedlib.cmt"])
           ];
         (* Run standard test. *)
         standard_test test_ctxt t;
         try_installed_library test_ctxt t "packedlib" ["Packedlib.Foo"]);

    (* Complete library with findlib package to check *)
    "examples/findlib" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_example_dir ["findlib"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         (* Run standard test. *)
         standard_test test_ctxt t);

    (* Complete library with custom build system *)
    "examples/custom" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_example_dir ["custom"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("simplelib",
                ["simplelib.cma";
                 "foo.cmi"; "foo.mli";
                 "bar.cmi"; "bar.mli";
                 "META"])
           ];
         (* Run standard test. *)
         standard_test test_ctxt t);

   (* TODO: reactivate *)
(*
    (* Library/executable using C files *)
    "examples/with-c" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_example_dir ["with-c"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            [
              "src/META";
              "src/libtest-with-c-custom_stubs.clib";
              "src/libtest-with-c-native_stubs.clib";
              "src/libtest-with-c_stubs.clib";
              "src/libwith-c_stubs.clib";
              "src/with-c.mllib";
              "src/with-c.mldylib";
              "src/with-c.odocl";
            ]);
        if is_native then
          register_installed_files test_ctxt t
            [InstalledBin ["test-with-c-native"]];
        register_installed_files test_ctxt t
          [
            InstalledBin ["test-with-c"; "test-with-c-custom"];
            InstalledLibrary ["with-c/dlltest-with-c_stubs.so"];
            InstalledOCamlLibrary
              ("with-c",
               ["A.cmi"; "A.ml"; "META"; "with-c.cma";
                "libwith-c_stubs.a"; "dllwith-c_stubs.so";
                "with-c.a"; "A.cmx"; "with-c.cmxa"; "with-c.cmxs";
                "A.annot"; "A.cmt"]);
            InstalledAPIRef("with-c", ["A"]);
          ];
        if OASISVersion.version_compare_string t.ocaml_version "4.00" < 0 then
          register_installed_files test_ctxt t
            [InstalledHTML("with-c", ["code_VALA.ident.html"])];
        (* Run standard test. *)
        standard_test test_ctxt t;
        (* Try the result. *)
        if is_native then
          try_installed_exec test_ctxt t "test-with-c-native" [];
        try_installed_exec test_ctxt t "test-with-c-custom" [];
        try_installed_exec test_ctxt t "test-with-c" [];
        try_installed_library test_ctxt t "with-c" ["A"]);

 *)
   (* TODO: reactivate *)
(*
    (* Library/executable using data files *)
    "examples/with-data" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_example_dir ["with-data"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            [
              "src/META";
              "src/test.mllib";
              "src/test.mldylib";
              "src/test.odocl";
            ]);
         register_installed_files test_ctxt t
           [
             InstalledBin ["test"];
             InstalledOCamlLibrary
               ("test",
                [
                  "test.ml"; "test.cmi"; "META"; "test.cma";
                  "test.annot"; "test.cmt"
                ]);
             InstalledData
               ["with-data/test.txt";
                "doc/html/test.html";
                "with-data-0.1/test.txt"];
             InstalledAPIRef("test", ["Test"]);
           ];
         (* Run standard test. *)
         standard_test test_ctxt t;
         (* Try the result. *)
         try_installed_library test_ctxt t "test" ["Test"]);
 *)

    (* Library with a pure interface module in subdirectory. *)
    "examples/with-interface-module" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_example_dir
           ["with-interface-module"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            [
              "src/META";
              "src/pimlib.mllib";
              "src/pimlib.mldylib";
            ]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("pimlib",
                ["META";
                 "pimlib.cma"; "pimlib.cmxa"; "pimlib.a"; "pimlib.cmxs";
                 "pim_impl.mli"; "pim_impl.cmi"; "pim_impl.cmx";
                 "pim_intf.mli"; "pim_intf.cmi";
                 "pim_types.mli"; "pim_types.cmi";
                 "pim_impl.annot"; "pim_impl.cmt"; "pim_impl.cmti";
                 "pim_intf.cmti"; "pim_types.cmti"]);
           ];
         (* Run standard test. *)
         standard_test test_ctxt t;
         (* Try the result. *)
         try_installed_library test_ctxt t "pimlib" ["Pim_intf"; "Pim_impl"]);

    (* Test executable *)
    "examples/with-test" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_example_dir ["with-test"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         (* Run standard test. *)
         standard_test test_ctxt t);
   (* TODO: reactivate *)
(*
    (* Use sub-packages *)
    "examples/with-subpackage" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_example_dir ["with-subpackage"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            [
              "src/META";
              "src/test.mllib";
              "src/test.mldylib";
              "src/test.odocl";
              "src/syntax/pa_test.mllib";
              "src/syntax/pa_test.mldylib";
            ]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("test",
                ["META"; "test.cma"; "pa_test.cma";
                 "A.ml"; "A.cmi"; "B.ml"; "B.cmi";
                 "pa_test.ml"; "pa_test.cmi";
                 "test.cmxa"; "test.cmxs"; "A.cmx"; "B.cmx";
                 "test.a";
                 "A.annot"; "A.cmt"; "B.annot"; "B.cmt";
                 "pa_test.annot"; "pa_test.cmt"]);
             InstalledAPIRef
               ("test", ["A"; "B"]);
           ];
         (* Run standard test. *)
         standard_test test_ctxt t;
         (* Try the result. *)
         try_installed_library test_ctxt t "test" ["A"; "B"]);
 *)

    (* Interdependencies *)
    "examples/interdepend-libraries" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt
           in_example_dir ["interdepend-libraries"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            [
              "src/interdepend.odocl";
              "src/liba/liba.mllib";
              "src/liba/liba.mldylib";
              "src/libb/libb.mllib";
              "src/libb/libb.mldylib";
              "src/libc/libc.mllib";
              "src/libc/libc.mldylib";
              "src/libd/libd.mllib";
              "src/libd/libd.mldylib";
              "src/libe/libe.mllib";
              "src/libe/libe.mldylib";
            ]);
         (* Run standard test. *)
         standard_test test_ctxt t);

    (* Build order *)
    "examples/order-matter" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_example_dir ["order-matter"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            [
              "src/order-matter.odocl";
              "src/foo/foo.mllib";
              "src/foo/foo.mldylib";
              "src/bar/bar.mllib";
              "src/bar/bar.mldylib";
              "src/baz/baz.mllib";
              "src/baz/baz.mldylib";
            ]);
         (* Run standard test. *)
         standard_test test_ctxt t);

    "examples/syntax-camlp4" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_example_dir ["syntax-camlp4"]
       in
       let () =
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            [
              "src/META";
              "src/pi.mllib";
              "src/pi.mldylib";
            ]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("syntax-camlp4",
                ["META"; "pi.ml"; "pi.cmi"; "pi.cma";
                 "pi.cmx"; "pi.cmxa"; "pi.cmxs";
                 "pi.a";
                 "pi.annot"; "pi.cmt"]);
           ];
         (* Run standard test. *)
         standard_test test_ctxt t;
         try_installed_library test_ctxt t "syntax-camlp4.syntax" []
       in
       (* Check what happens when link all is set. *)
       let srcdir = bracket_tmpdir test_ctxt in
       let fn = FilePath.concat srcdir "foo.ml" in
       let exec_byte = FilePath.replace_extension fn "byte" in
         FileUtil.cp
           [in_example_dir test_ctxt
              ["syntax-camlp4"; "test"; "data"; "foo.ml"]]
           fn;
         assert_compile test_ctxt t "syntax-camlp4.syntax"
           "ocamlc" ["-o"; exec_byte;
                     "-linkall"; "-linkpkg"; "-syntax"; "camlp4o"; fn];
         assert_bool
           ("Executable is not linked with camlp4")
           (not (contains_string fn "Camlp4")));


   (* TODO: reactivate *)
(*
    (* Single level package *)
    "1level" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["TestFull"; "1level"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            [
              "META";
              "with-a.mllib";
              "with-a.mldylib";
              "with-a.odocl";
            ]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("with-a",
                ["META"; "A.ml"; "A.cmi"; "with-a.cma";
                 "A.cmx"; "with-a.cmxa"; "with-a.cmxs";
                 "with-a.a";
                 "A.annot"; "A.cmt"]);
             InstalledBin ["test-with-a"];
             InstalledAPIRef("with-a", ["A"]);
           ];
         (* Run standard test. *)
         standard_test test_ctxt t;
         (* Try the result. *)
         try_installed_library test_ctxt t "with-a" ["A"];
         try_installed_exec test_ctxt t "test-with-a" []);
 *)

   (* TODO: reactivate *)
(*
    (* Try custom document build *)
    "customdoc" >::
    (fun test_ctxt ->
       (* TODO: check custom install as well. *)
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir
           ["TestFull"; "customdoc"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            ["META"; "with-a.mllib"; "with-a.mldylib"]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("with-a",
                ["META"; "A.ml"; "A.cmi"; "A.annot"; "A.cmti"; "with-a.cma"]);
           ];
         (* Run standard test. *)
         standard_test test_ctxt t);
 *)

    (* Use cclib option *)
    "with-cclib" >::
    (fun test_ctxt ->
       let () =
         skip_long_test test_ctxt;
         skip_if
           (not (Sys.file_exists "/usr/include/stringprep.h"))
           "Cannot find 'stringprep.h'"
       in
       let t =
         setup_test_directories test_ctxt in_testdata_dir
           ["TestFull"; "with-cclib"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            [
              "src/META";
              "Makefile";
              "configure";
              "src/libtest_oasis_c_dependency_stubs.clib";
              "src/test_oasis_c_dependency.mllib";
              "src/test_oasis_c_dependency.mldylib";
            ]);
         (* Run standard test. *)
         standard_test test_ctxt t);

    (* With a documentation that is not built *)
    "no-install-doc" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir
           ["TestFull"; "no-install-doc"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         (* Run standard test. *)
         standard_test test_ctxt t);

    (* Need to create a a parent directory *)
    "create-parent-dir" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir
           ["TestFull"; "create-parent-dir"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         register_installed_files test_ctxt t
           [InstalledData ["toto/toto/toto.txt"]];
         (* Run standard test. *)
         standard_test test_ctxt t);

   (* TODO: reactivate *)
(*
    (* TODO: move full tests under their own directory. *)
    "bug588" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let () =
         let cmd =
           Printf.sprintf
             "ocamlfind query bitstring > %s 2>&1"
             (if Sys.os_type = "Win32" then
                "NUL"
              else
                "/dev/null")
         in
           skip_if
             (Sys.command cmd <> 0)
             "Cannot find package bitstring"
       in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["TestFull"; "bug588"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           ((List.filter (( <> ) "_tags") oasis_ocamlbuild_files) @
            ["libtest.mllib"; "libtest.mldylib"; "libtest.odocl"]);
         (* Run standard test. *)
         standard_test test_ctxt t);
 *)

    "bug619" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["TestFull"; "bug619"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         (* Run standard test. *)
         standard_test test_ctxt t);

    "bug571" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["TestFull"; "bug571"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         (* Run standard test. *)
         standard_test test_ctxt t);

    "flag-ccopt" >::
    (fun test_ctxt ->
       let () =
         skip_long_test test_ctxt;
         skip_if
           (not (Sys.file_exists "/usr/lib/libz.so.1"))
           "zlib not installed"
       in
       let t =
         setup_test_directories test_ctxt in_testdata_dir
           ["TestFull"; "flag-ccopt"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            ["cryptokit.mllib"; "cryptokit.mldylib"]);
         (* Run standard test. *)
         standard_test test_ctxt t);

    "bug738" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["TestFull"; "bug738"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            ["src/test.mllib"; "src/test.mldylib"; "src/META"]);
         register_installed_files test_ctxt t
           [InstalledOCamlLibrary ("test", ["META"; "foo.cmi"; "test.cma";
                                   "foo.annot"; "foo.cmt"])];
         (* Run standard test. *)
         standard_test test_ctxt t);

    "bug982" >::
    (fun test_ctxt ->
       let () =
         skip_long_test test_ctxt;
         skip_if (Sys.os_type = "Win32") "UNIX only test"
       in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["TestFull"; "bug982"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         (* Run standard test. *)
         standard_test test_ctxt t);

   "bug823" >::
    (fun test_ctxt ->
       let () =
         skip_long_test test_ctxt;
         skip_if (Sys.os_type = "Win32") "UNIX only test"
       in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["TestFull"; "bug823"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         (* Run standard test. *)
         standard_test test_ctxt t);

   (* TODO: reactivate *)
(*
   "bugClib" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir
           ["TestFull"; "bugClib"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            ["META"; "mylib.mlpack"; "mylib.mllib"; "mylib.mldylib";
             "libmylib_stubs.clib"]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("mylib",
                ["META"; "dllmylib_stubs.so";
                 "foo.ml"; "mylib.cma"; "mylib.cmi";
                 "foo.annot"; "foo.cmt"; "foo.cmti";
                 "bar.annot"; "bar.cmt"; "bar.cmti";
                 "mylib.cmxa"; "mylib.cmxs"; "mylib.cmx";
                 "mylib.a"; "libmylib_stubs.a"])
           ];
         (* Run standard test. *)
         standard_test test_ctxt t;
         (* Try the result. *)
         try_installed_library test_ctxt t "mylib" ["Mylib.Foo"; "Mylib.Bar"]);
 *)

    "bug791" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir
           ["TestFull"; "bug791"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @ ["src/testA.mllib"; "src/testA.mldylib"]);
         (* Run standard test. *)
         standard_test test_ctxt t);

    "examples/object" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_example_dir ["object"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            ["src1/META"; "src2/META"; "src2/packed_modules.mlpack"]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("single_module",
                ["META"; "single.o" ; "single.mli"; "single.cmx"; "single.cmo";
                 "single.cmi"]);
             InstalledOCamlLibrary
               ("packed_modules",
                ["packed_modules.o"; "packed_modules.cmx";
                 "packed_modules.cmo"; "packed_modules.cmi";
                 "m1.ml"; "m2.mli"; "META"]);
           ];
         (* Run standard test. *)
         standard_test test_ctxt t);

    "bug938">::
    (fun test_ctxt ->
       let () =
         skip_if (Sys.os_type = "Win32") "UNIX test"
       in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["TestFull"; "bug938"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         (* Check everything ok. *)
         standard_checks test_ctxt t;
         (* Try to run. *)
         (* TODO: quid of the use of this test. Check and comment. *)
         run_ocaml_setup_ml test_ctxt t
           ["-configure"; "--enable-all"; "--disable-over"]);

    "TEMP=a b" >::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["TestFull"; "bug571"]
       in
       let tmpdir = bracket_tmpdir test_ctxt in
       let dn = Filename.concat tmpdir "a b" in
         mkdir dn;
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         (* Check everything ok. *)
         standard_checks test_ctxt t;
         (* Run the test, try to use a temporary directory with space in the
          * filename. *)
         run_ocaml_setup_ml test_ctxt t
           ~extra_env:[if Sys.os_type = "Win32" then
                         "TEMP", dn
                       else
                         "TMPDIR", dn]
           ["-configure"]);

    "setup with dev mode (weak)">::
    (fun test_ctxt ->
       let () =
         skip_if (Sys.os_type = "Win32") "UNIX test"
       in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["TestFull"; "dev"]
       in
         (* Copy initial version of the _oasis. *)
         cp [in_src_dir t "_oasis.v1"] (in_src_dir t "_oasis");
         oasis_setup ~dev:true test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t ("_oasis" :: oasis_ocamlbuild_files);
         (* Check everything ok. *)
         standard_checks test_ctxt t;
         (* Run test. *)
         run_ocaml_setup_ml test_ctxt t ["-all"];
         assert_bool
           "Library .cma not created."
           (not (Sys.file_exists (in_src_dir t "_build/mylib.cma")));
         cp [in_src_dir t "_oasis.v2"] (in_src_dir t "_oasis");
         run_ocaml_setup_ml test_ctxt t ["-all"];
         assert_bool
           "Library .cma created."
           (Sys.file_exists (in_src_dir t "_build/mylib.cma")));

    "setup with dev mode (light)">::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["TestFull"; "dev"]
       in
         (* Copy initial version of the _oasis. *)
         cp [in_src_dir t "_oasis.v2"] (in_src_dir t "_oasis");
         oasis_setup ~dev:true ~dynamic:true test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t ["_oasis"];
         (* Check everything ok. *)
         standard_checks test_ctxt t;
         (* Run test. *)
         assert_bool
           "setup.ml is smaller than 3kB"
           (let chn = open_in (in_src_dir t setup_ml) in
              try
                let size = in_channel_length chn in
                  close_in chn;
                  size < 3072 (* 3kB *)
              with e ->
                close_in chn;
                raise e);
         run_ocaml_setup_ml test_ctxt t ["-all"];
         assert_bool
           "Library .cma created."
           (Sys.file_exists (in_src_dir t "_build/mylib.cma")));

    "setup with no dev mode">::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["TestFull"; "dev"]
       in
         (* Copy initial version of the _oasis. *)
         cp [in_src_dir t "_oasis.v1"] (in_src_dir t "_oasis");
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t ("_oasis" :: oasis_ocamlbuild_files);
         (* Check everything ok. *)
         standard_checks test_ctxt t;
         (* Run test. *)
         run_ocaml_setup_ml test_ctxt t ["-all"];
         assert_bool
           "Library .cma not created."
           (not (Sys.file_exists (in_src_dir t "_build/mylib.cma")));
         cp [in_src_dir t "_oasis.v2"] (in_src_dir t "_oasis");
         run_ocaml_setup_ml test_ctxt t ["-all"];
         assert_bool
           "Library .cma still not created."
           (not (Sys.file_exists (in_src_dir t "_build/mylib.cma"))));

    "ver0.3">::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["TestFull"; "ver0.3"]
       in
       let doc_done_fn = in_src_dir t "doc-done" in
       let test_done_fn = in_src_dir t "test-done" in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         (* Check everything ok. *)
         standard_checks test_ctxt t;
         (* Run test. *)
         run_ocaml_setup_ml test_ctxt t ["-configure"];
         run_ocaml_setup_ml test_ctxt t ["-test"];
         assert_bool
           "test not run."
           (not (Sys.file_exists test_done_fn));
         run_ocaml_setup_ml test_ctxt t ["-doc"];
         assert_bool
           "doc done."
           (Sys.file_exists doc_done_fn);
         run_ocaml_setup_ml test_ctxt t ["-distclean"];
         rm [doc_done_fn; test_done_fn];


         run_ocaml_setup_ml test_ctxt t
           ["-configure"; "--enable-tests"; "--disable-docs"];
         run_ocaml_setup_ml test_ctxt t ["-test"];
         assert_bool
           "test run."
           (Sys.file_exists test_done_fn);
         run_ocaml_setup_ml test_ctxt t ["-doc"];
         assert_bool
           "doc not done."
           (not (Sys.file_exists doc_done_fn));
         run_ocaml_setup_ml test_ctxt t ["-distclean"]);

    "bug1358">::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt in_testdata_dir
           ["TestFull"; "bug1358"]
       in
       let () =
         skip_if
           (OASISVersion.version_compare_string t.ocaml_version "4.00" < 0)
           "OCaml >= 4.00 needed."
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         (* Run standard test. *)
         standard_test test_ctxt t);

    "dynrun_for_release">::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt in_testdata_dir
           ["TestFull"; "dynrun_for_release"]
       in
         oasis_setup ~dev:true ~dynamic:true test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t ["INSTALL.txt"];
         (* Run standard test. *)
         standard_test test_ctxt t;
         dbug_file_content test_ctxt (in_src_dir t "INSTALL.txt"));

    "dynlink">::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt in_testdata_dir
           ["TestFull"; "dynlink"]
       in
       let has_native = is_native && (native_dynlink test_ctxt) in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            ["META"; "entry_point.mllib"; "entry_point.mldylib";
             "dyn_loaded.mllib"; "dyn_loaded.mldylib"]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("entry_point",
                ["META";
                 "entry_point.cma"; "entry_point.cmi";
                 "entry_point.cmxa"; "entry_point.cmxs";
                 "entry_point.cmx"; "entry_point.a";
                 "entry_point.ml";
                 "dyn_loaded.cma"; "dyn_loaded.cmxa";
                 "dyn_loaded.cmxs"; "dyn_loaded.a";
                 "dyn_loaded.cmx"; "dyn_loaded.cmi";
                 "dyn_loaded.ml";
                 "dyn_loaded_ext.cmx"; "dyn_loaded_ext.cmi";
                 "dyn_loaded_ext.ml";
                 "dyn_loaded.annot"; "dyn_loaded.cmt";
                 "dyn_loaded_ext.annot"; "dyn_loaded_ext.cmt";
                 "entry_point.annot"; "entry_point.cmt";
                ]);
             InstalledBin ["dynlink-test-byte"];
           ];
         if has_native then
           register_installed_files test_ctxt t
             [InstalledBin ["dynlink-test-native"]];
         (* Run standard test. *)
         standard_test test_ctxt t;
         (* Try the result. *)
         try_installed_exec test_ctxt t "dynlink-test-byte"
           ["-load";
            FilePath.make_filename
              [t.ocaml_lib_dir; "entry_point"; "dyn_loaded.cma"]];
         if has_native then
           try_installed_exec test_ctxt t "dynlink-test-native"
             ["-load";
              FilePath.make_filename
                [t.ocaml_lib_dir; "entry_point"; "dyn_loaded.cmxs"]];
         ());

    "recurselib">::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt in_testdata_dir
           ["TestFull"; "recurselib"]
       in
         (* The test is that 'oasis setup' should not fail. *)
         oasis_setup test_ctxt t);
  ]


let tests =
  "TestFull" >:::
  [
    "best=native" >:::
    (skip_test_on_non_native_arch
       (gen_tests ~is_native:true ()));

    "best=byte" >:::
    (gen_tests ~is_native:false ());
  ]
