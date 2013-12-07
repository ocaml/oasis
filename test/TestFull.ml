(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2008-2010, OCamlCore SARL                                    *)
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
              "src/simplelib/simplelib.odocl";
              "src/simplelibext/simplelibext.mllib";
              "src/simplelibext/simplelibext.odocl";
            ]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("simplelib",
                ["simplelib.cma";
                 "Foo.cmi"; "Foo.ml";
                 "Bar.cmi"; "Bar.ml";
                 "META";
                 "simplelib.cmxa";
                 "simplelib.cmxs";
                 "Foo.cmx"; "Bar.cmx";
                 "simplelib.a"]);
             InstalledOCamlLibrary
               ("simplelibext",
                ["simplelibext.cma";
                 "FooExt.cmi"; "FooExt.ml";
                 "BarExt.cmi"; "BarExt.ml";
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
              "src/simplelib.odocl";
            ]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("simplelib" ,
                ["simplelib.cma";
                 "foo.cmi"; "foo.mli";
                 "bar.cmi"; "bar.mli";
                 "META";
                 "simplelib.cmxa";
                 "simplelib.cmxs";
                 "foo.cmx"; "bar.cmx";
                 "simplelib.a"]);
             InstalledAPIRef("simplelib", ["Bar"; "Foo"]);
           ];
         (* Run standard test. *)
         standard_test test_ctxt t);

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
            ["src/packedlib.mlpack"; "src/packedlib.mllib"; "src/META"]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("packedlib",
                ["packedlib.cma"; "packedlib.cmi"; "packedlib.cmx";
                 "foo.mli"; "bar.mli"; "Baz.ml"; "META";
                 "packedlib.cmxa"; "packedlib.cmxs";
                 "packedlib.a"])
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
                "with-c.a"; "A.cmx"; "with-c.cmxa"; "with-c.cmxs"]);
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
              "src/test.odocl";
            ]);
         register_installed_files test_ctxt t
           [
             InstalledBin ["test"];
             InstalledOCamlLibrary
               ("test",
                [
                  "test.ml"; "test.cmi"; "META"; "test.cma";
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
            ]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("pimlib",
                ["META";
                 "pimlib.cma"; "pimlib.cmxa"; "pimlib.a"; "pimlib.cmxs";
                 "pim_impl.mli"; "pim_impl.cmi"; "pim_impl.cmx";
                 "pim_intf.mli"; "pim_intf.cmi";
                 "pim_types.mli"; "pim_types.cmi"]);
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
              "src/test.odocl";
              "src/syntax/pa_test.mllib";
            ]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("test",
                ["META"; "test.cma"; "pa_test.cma";
                 "A.ml"; "A.cmi"; "B.ml"; "B.cmi";
                 "pa_test.ml"; "pa_test.cmi";
                 "test.cmxa"; "test.cmxs"; "A.cmx"; "B.cmx";
                 "test.a"]);
             InstalledAPIRef
               ("test", ["A"; "B"]);
           ];
         (* Run standard test. *)
         standard_test test_ctxt t;
         (* Try the result. *)
         try_installed_library test_ctxt t "test" ["A"; "B"]);

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
              "src/libb/libb.mllib";
              "src/libc/libc.mllib";
              "src/libd/libd.mllib";
              "src/libe/libe.mllib";
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
              "src/bar/bar.mllib";
              "src/baz/baz.mllib";
            ]);
         (* Run standard test. *)
         standard_test test_ctxt t);

    (* Single level package *)
    "1level" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["1level"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            [
              "META";
              "with-a.mllib";
              "with-a.odocl";
            ]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("with-a",
                ["META"; "A.ml"; "A.cmi"; "with-a.cma";
                 "A.cmx"; "with-a.cmxa"; "with-a.cmxs";
                 "with-a.a"]);
             InstalledBin ["test-with-a"];
             InstalledAPIRef("with-a", ["A"]);
           ];
         (* Run standard test. *)
         standard_test test_ctxt t;
         (* Try the result. *)
         try_installed_library test_ctxt t "with-a" ["A"];
         try_installed_exec test_ctxt t "test-with-a" []);

    (* Try custom document build *)
    "customdoc" >::
    (fun test_ctxt ->
       (* TODO: check custom install as well. *)
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["customdoc"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @ ["META"; "with-a.mllib"]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("with-a",
                ["META"; "A.ml"; "A.cmi"; "with-a.cma"]);
           ];
         (* Run standard test. *)
         standard_test test_ctxt t);

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
         setup_test_directories test_ctxt in_testdata_dir ["with-cclib"]
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
            ]);
         (* Run standard test. *)
         standard_test test_ctxt t);

    (* With a documentation that is not built *)
    "no-install-doc" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["no-install-doc"]
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
         setup_test_directories test_ctxt in_testdata_dir ["create-parent-dir"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         register_installed_files test_ctxt t
           [InstalledData ["toto/toto/toto.txt"]];
         (* Run standard test. *)
         standard_test test_ctxt t);

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
         setup_test_directories test_ctxt in_testdata_dir ["bug588"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           ((List.filter (( <> ) "_tags") oasis_ocamlbuild_files) @
            ["libtest.mllib"; "libtest.odocl"]);
         (* Run standard test. *)
         standard_test test_ctxt t);

    "bug619" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["bug619"]
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
         setup_test_directories test_ctxt in_testdata_dir ["bug571"]
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
         setup_test_directories test_ctxt in_testdata_dir ["flag-ccopt"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @ ["cryptokit.mllib"]);
         (* Run standard test. *)
         standard_test test_ctxt t);

    "bug738" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["bug738"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @ ["src/test.mllib"; "src/META"]);
         register_installed_files test_ctxt t
           [InstalledOCamlLibrary ("test", ["META"; "foo.cmi"; "test.cma"])];
         (* Run standard test. *)
         standard_test test_ctxt t);

    "bug982" >::
    (fun test_ctxt ->
       let () =
         skip_long_test test_ctxt;
         skip_if (Sys.os_type = "Win32") "UNIX only test"
       in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["bug982"]
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
         setup_test_directories test_ctxt in_testdata_dir ["bug823"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         (* Run standard test. *)
         standard_test test_ctxt t);

   "bugClib" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["bugClib"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            ["META"; "mylib.mlpack"; "mylib.mllib"; "libmylib_stubs.clib"]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("mylib",
                ["META"; "dllmylib_stubs.so";
                 "foo.ml"; "mylib.cma"; "mylib.cmi";
                 "mylib.cmxa"; "mylib.cmxs"; "mylib.cmx";
                 "mylib.a"; "libmylib_stubs.a"])
           ];
         (* Run standard test. *)
         standard_test test_ctxt t;
         (* Try the result. *)
         try_installed_library test_ctxt t "mylib" ["Mylib.Foo"; "Mylib.Bar"]);

    "bug791" >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt in_testdata_dir ["bug791"]
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @ ["src/testA.mllib"]);
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
         setup_test_directories test_ctxt in_testdata_dir ["bug938"]
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
         setup_test_directories test_ctxt in_testdata_dir ["bug571"]
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
         setup_test_directories test_ctxt in_testdata_dir ["dev"]
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
         setup_test_directories test_ctxt in_testdata_dir ["dev"]
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
           "setup.ml is smaller than 2kB"
           (let chn = open_in (in_src_dir t setup_ml) in
              try
                let size = in_channel_length chn in
                  close_in chn;
                  size < 2048 (* 2kB *)
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
         setup_test_directories test_ctxt in_testdata_dir ["dev"]
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
         setup_test_directories test_ctxt in_testdata_dir ["ver0.3"]
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
  ]


let tests =
  let skip_test_on_non_native_arch lst =
    let skip_non_native =
      OUnitTest.test_decorate
        (fun f ->
           fun test_ctxt ->
             skip_if
               (* Use the real is_native function and skip if on non native
                * arch.
                *)
               (not (is_native test_ctxt))
               "only run on native arch";
             f test_ctxt)
    in
      List.map skip_non_native lst
  in

  "TestFull" >:::
  [
    "best=native" >:::
    (skip_test_on_non_native_arch
       (gen_tests ~is_native:true ()));

    "best=byte" >:::
    (gen_tests ~is_native:false ());
  ]
