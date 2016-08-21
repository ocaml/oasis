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


(** End to end tests for examples/ sub-directories. 
  
  Tests in this file run end to end compilation with OASIS for sub-directories
  found in examples/.
  
  @author Sylvain Le Gall
 *)

open FileUtil
open OUnit2
open TestCommon
open TestFullUtils

let all_tests = 
  [
    "flags",
    (fun test_ctxt t ->
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
               "Foo.cmi"; "Foo.ml"; "Foo.annot"; "Foo.cmt";
               "Bar.cmi"; "Bar.ml"; "Bar.annot"; "Bar.cmt";
               "META";
               "simplelib.cmxa";
               "simplelib.cmxs";
               "Foo.cmx"; "Bar.cmx";
               "simplelib.a"]);
           InstalledOCamlLibrary
             ("simplelibext",
              ["simplelibext.cma";
               "FooExt.cmi"; "FooExt.ml"; "FooExt.annot";
               "FooExt.cmt";
               "BarExt.cmi"; "BarExt.ml"; "BarExt.annot";
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

    (* Complete library *)
    "simplelib",
    (fun test_ctxt t ->
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

    (* Packed library *)
    "packedlib",
    (fun test_ctxt t ->
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
               "foo.annot"; "foo.cmt"; "foo.cmti"])
         ];
       if OASISVersion.StringVersion.compare t.ocaml_version "4.02" >= 0 then begin
         register_installed_files test_ctxt t [InstalledOCamlLibrary("packedlib", ["packedlib.cmt"])]
       end;
       (* Run standard test. *)
       standard_test test_ctxt t;
       try_installed_library test_ctxt t "packedlib" ["Packedlib.Foo"]);

    (* Complete library with findlib package to check *)
    "findlib",
    (fun test_ctxt t ->
       oasis_setup test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t oasis_ocamlbuild_files;
       (* Run standard test. *)
       standard_test test_ctxt t);

    (* Complete library with custom build system *)
    "custom",
    (fun test_ctxt t ->
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
    "with-c",
    (fun test_ctxt t ->
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
      if t.is_native then
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
      if OASISVersion.StringVersion.compare t.ocaml_version "4.00" < 0 then
        register_installed_files test_ctxt t
          [InstalledHTML("with-c", ["code_VALA.ident.html"])];
      (* Run standard test. *)
      standard_test test_ctxt t;
      (* Try the result. *)
      if t.is_native then
        try_installed_exec test_ctxt t "test-with-c-native" [];
      try_installed_exec test_ctxt t "test-with-c-custom" [];
      try_installed_exec test_ctxt t "test-with-c" [];
      try_installed_library test_ctxt t "with-c" ["A"]);

    (* Library/executable using data files *)
    "with-data",
    (fun test_ctxt t ->
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

    (* Library with a pure interface module in subdirectory. *)
    "with-interface-module",
    (fun test_ctxt t ->
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
    "with-test",
    (fun test_ctxt t ->
       oasis_setup test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t oasis_ocamlbuild_files;
       (* Run standard test. *)
       standard_test test_ctxt t);

    (* Use sub-packages *)
    "with-subpackage",
    (fun test_ctxt t ->
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

    (* Interdependencies *)
    "interdepend-libraries",
    (fun test_ctxt t ->
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
    "order-matter",
    (fun test_ctxt t ->
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

    "syntax-camlp4",
    (fun test_ctxt t ->
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

    "object",
    (fun test_ctxt t ->
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

    "ocamlbuild",
    (fun test_ctxt t ->
       oasis_setup test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t
         (oasis_ocamlbuild_files @ ["src/test.mldylib"; "src/test.mllib"; "src/META"]);
       register_installed_files test_ctxt t
         [
           InstalledOCamlLibrary
             ("test",
              ["test.ml"; "test.a"; "test.annot"; "test.cma"; "test.cmi"; "test.cmt";
               "test.cmx"; "test.cmxa"; "test.cmxs"; "META"]);
         ];
       (* Run standard test. *)
       standard_test test_ctxt t);
  ]


let gen_tests ~is_native () =
  let runner (nm, f) =
    ("examples/"^nm) >::
    (fun test_ctxt ->
      let () = skip_long_test test_ctxt in
      let t =
        setup_test_directories test_ctxt ~is_native
          ~native_dynlink:is_native
          (in_example_dir test_ctxt [nm])
      in
      let pkg =
        OASISParse.from_file
          ~ctxt:(oasis_ctxt test_ctxt)
          (in_src_dir t OASISParse.default_oasis_fn)
      in
        assert_equal
          ~msg:"Latest OASIS version"
          ~cmp:(fun v1 v2 -> OASISVersion.version_compare v1 v2 = 0)
          OASISConf.version_short pkg.OASISTypes.oasis_version;
        f test_ctxt t)
  in
    List.map runner all_tests


let tests =
  "TestExamples" >:::
  [
    "best=native">:::
    (skip_test_on_non_native_arch
       (gen_tests ~is_native:true ()));

    "best=byte">:::
    (gen_tests ~is_native:false ());

    "all_examples">::
    (fun test_ctxt ->
       all_subdirectories test_ctxt
         (in_example_dir test_ctxt [])
         ("oasis" :: List.map fst all_tests)
         (Printf.sprintf "examples/%s is not tested."));

    "examples/oasis">::
    (fun test_ctxt ->
       let dn = in_example_dir test_ctxt ["oasis"] in
         Array.iter
           (fun bn ->
              let fn = Filename.concat dn bn in
                non_fatal test_ctxt
                  (fun test_ctxt ->
                     try
                       let _pkg: OASISTypes.package =
                         OASISParse.from_file ~ctxt:(oasis_ctxt test_ctxt) fn
                       in
                         ()
                     with e ->
                       assert_failure
                         (Printf.sprintf "Parsing error of %s: %s."
                            ("oasis/"^bn) (Printexc.to_string e))))
           (Sys.readdir dn));
  ]
