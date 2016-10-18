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


(** Run full OASIS use cases
    @author Sylvain Le Gall
  *)


open FileUtil
open OUnit2
open TestCommon
open TestFullUtils


let all_tests =
  [
    (* Single level package *)
    "1level",
    (fun test_ctxt t ->
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

    (* Try custom document build *)
    "customdoc",
    (fun test_ctxt t ->
       (* TODO: check custom install as well. *)
       oasis_setup test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t
         (oasis_ocamlbuild_files @
          ["META"; "with-a.mllib"; "with-a.mldylib"]);
       register_installed_files test_ctxt t
         [
           InstalledOCamlLibrary
             ("with-a",
              ["META"; "A.ml"; "A.cmi"; "A.annot"; "A.cmt"; "with-a.cma"]);
         ];
       (* Run standard test. *)
       standard_test test_ctxt t);

    (* Use cclib option *)
    "with-cclib",
    (fun test_ctxt t ->
       skip_if
         (not (Sys.file_exists "/usr/include/stringprep.h"))
         "Cannot find 'stringprep.h'";
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
    "no-install-doc" ,
    (fun test_ctxt t ->
       oasis_setup test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t oasis_ocamlbuild_files;
       (* Run standard test. *)
       standard_test test_ctxt t);

    (* Need to create a a parent directory *)
    "create-parent-dir",
    (fun test_ctxt t ->
       oasis_setup test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t oasis_ocamlbuild_files;
       register_installed_files test_ctxt t
         [InstalledData ["toto/toto/toto.txt"]];
       (* Run standard test. *)
       standard_test test_ctxt t);

    "bug588",
    (fun test_ctxt t ->
       (* Copy initial version of the _oasis. *)
       cp [in_src_dir t "_tags_manual"] (in_src_dir t "_tags");
       oasis_setup test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t
         (oasis_ocamlbuild_files @
          ["libtest.mllib"; "libtest.mldylib"; "libtest.odocl"]);
       (* Run standard test. *)
       standard_test test_ctxt t);

    "bug619",
    (fun test_ctxt t ->
       oasis_setup test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t oasis_ocamlbuild_files;
       (* Run standard test. *)
       standard_test test_ctxt t);

    "bug571",
    (fun test_ctxt t ->
       oasis_setup test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t oasis_ocamlbuild_files;
       (* Run standard test. *)
       standard_test test_ctxt t);

    "flag-ccopt",
    (fun test_ctxt t ->
       let () =
         skip_if (Sys.os_type = "Win32") "UNIX only test";
         skip_if
           (Sys.command "pkg-config zlib --exists" <> 0)
           "zlib not installed"
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t
           (oasis_ocamlbuild_files @
            ["cryptokit.mllib"; "cryptokit.mldylib"]);
         (* Run standard test. *)
         standard_test test_ctxt t);

    "bug738",
    (fun test_ctxt t ->
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

    "bug982",
    (fun test_ctxt t ->
       let () =
         skip_if (Sys.os_type = "Win32") "UNIX only test"
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         (* Run standard test. *)
         standard_test test_ctxt t);

   "bug823",
    (fun test_ctxt t ->
       let () =
         skip_if (Sys.os_type = "Win32") "UNIX only test"
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         (* Run standard test. *)
         standard_test test_ctxt t);

   "bugClib",
    (fun test_ctxt t ->
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
               "foo.annot"; "foo.cmt";
               "bar.annot"; "bar.cmt";
               "mylib.cmxa"; "mylib.cmxs"; "mylib.cmx";
               "mylib.a"; "libmylib_stubs.a"])
         ];
       if OASISVersion.StringVersion.compare t.ocaml_version "4.02" >= 0 then begin
         register_installed_files
           test_ctxt
           t
           [InstalledOCamlLibrary("mylib", ["mylib.cmt"])]
       end;
       (* Run standard test. *)
       standard_test test_ctxt t;
       (* Try the result. *)
       try_installed_library test_ctxt t "mylib" ["Mylib.Foo"; "Mylib.Bar"]);

    "bug791",
    (fun test_ctxt t ->
       oasis_setup test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t
         (oasis_ocamlbuild_files @ ["src/testA.mllib"; "src/testA.mldylib"]);
       (* Run standard test. *)
       standard_test test_ctxt t);

    "bug938",
    (fun test_ctxt t ->
       let () =
         skip_if (Sys.os_type = "Win32") "UNIX test"
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

    "ver0.3",
    (fun test_ctxt t ->
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

    "bug1358",
    (fun test_ctxt t ->
       let () =
         skip_if
           (OASISVersion.StringVersion.compare t.ocaml_version "4.00" < 0)
           "OCaml >= 4.00 needed."
       in
         oasis_setup test_ctxt t;
         (* Setup expectation. *)
         register_generated_files t oasis_ocamlbuild_files;
         (* Run standard test. *)
         standard_test test_ctxt t);

    "dynrun_for_release",
    (fun test_ctxt t ->
       oasis_setup ~dev:true ~dynamic:true test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t ["INSTALL.txt"];
       (* Run standard test. *)
       standard_test test_ctxt t;
       dbug_file_content test_ctxt (in_src_dir t "INSTALL.txt"));

    "dynlink",
    (fun test_ctxt t ->
       let has_native = t.is_native && (native_dynlink test_ctxt) in
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

    "recurselib",
    (fun test_ctxt t ->
       (* The test is that 'oasis setup' should not fail. *)
       oasis_setup test_ctxt t);

    "cppcc",
    (fun test_ctxt t ->
       oasis_setup test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t 
         (oasis_ocamlbuild_files @
          ["META"; "cppcc.mldylib"; "cppcc.mllib"; "libcppcc_stubs.clib"]);
       register_installed_files test_ctxt t
         [
           InstalledOCamlLibrary
             ("cppcc",
              ["META"; "cppcc.a"; "cppcc.annot"; "cppcc.cma"; "cppcc.cmi";
               "cppcc.cmt"; "cppcc.cmx"; "cppcc.cmxa"; "cppcc.cmxs";
               "cppcc.ml"; "dllcppcc_stubs.so"; "libcppcc_stubs.a";
              ]);
         ];
       (* Run standard test. *)
       standard_test test_ctxt t);

    "bug1239",
    (fun test_ctxt t ->
       oasis_setup test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t
         (oasis_ocamlbuild_files @
          ["src/META"; "src/bar/META"; "src/bar/bar.mldylib";
           "src/bar/bar.mllib"; "src/bar/bar.mlpack"; "src/foo.mldylib";
           "src/foo.mllib";
          ]);
       register_installed_files test_ctxt t
         [
           InstalledOCamlLibrary
             ("bar",
              ["META"; "a.annot"; "a.cmt"; "a.ml"; "bar.a"; "bar.cma";
               "bar.cmi"; "bar.cmx"; "bar.cmxa"; "bar.cmxs";
              ]);
           InstalledOCamlLibrary
             ("foo",
              ["META"; "foo.a"; "foo.cma"; "foo.cmxa"; "foo.cmxs"; "m.annot";
               "m.cmi"; "m.cmt"; "m.ml"; "m.cmx";
              ]);
         ];
       if OASISVersion.StringVersion.compare t.ocaml_version "4.02" >= 0 then begin
         register_installed_files
           test_ctxt
           t
           [InstalledOCamlLibrary("bar", ["bar.cmt"])]
       end;
       (* Run standard test. *)
       standard_test test_ctxt t);

    "bug623",
    (fun test_ctxt t ->
       oasis_setup test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t
         (oasis_ocamlbuild_files @
          ["empty.mldylib"; "empty.mllib"]);
       (* Run standard test. *)
       run_ocaml_setup_ml test_ctxt t ["-configure"; "--enable-tests"];
       run_ocaml_setup_ml test_ctxt t ~exit_code:(Unix.WEXITED 1) ["-test"]);

    (* Use -C to change directory in 'ocaml setup.ml'. *)
    "bug1473",
    (fun test_ctxt t ->
       let tmpdir = bracket_tmpdir test_ctxt in
       let hfs = new OASISFileSystem.host_fs t.src_dir in
       let setup_log = BaseLog.default_filename in
       let setup_data = BaseEnv.default_filename in
       oasis_setup test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t
         (oasis_ocamlbuild_files @
          ["empty.mldylib"; "empty.mllib"]);
       assert_command
         ~chdir:tmpdir
         ~ctxt:test_ctxt
         "ocaml"
         [Filename.concat t.src_dir setup_ml; "-C"; t.src_dir; "-configure"];
       assert_command
         ~chdir:tmpdir
         ~ctxt:test_ctxt
         "ocaml"
         [Filename.concat t.src_dir setup_ml; "-C"; t.src_dir; "-build"];
       assert_equal
         ~msg:(Printf.sprintf "Temporary directory %S should be empty." tmpdir)
         ~printer:(fun a -> "["^(String.concat "; " (Array.to_list a))^"]")
         [||]
         (Sys.readdir tmpdir);
       assert_bool
         (Printf.sprintf
            "File %S and %S should exist."
            (hfs#string_of_filename setup_log)
            (hfs#string_of_filename setup_data))
         (hfs#file_exists setup_log && hfs#file_exists setup_data));
  ]


let different_directory_tests =
  [
    "TEMP=a b", "bug571",
    (fun test_ctxt t ->
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

    "setup with dev mode (weak)", "dev",
    (fun test_ctxt t ->
       let () =
         skip_if (Sys.os_type = "Win32") "UNIX test"
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

    "setup with dev mode (light)", "dev",
    (fun test_ctxt t ->
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

    "setup with no dev mode", "dev",
    (fun test_ctxt t ->
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
  ]


let gen_tests ~is_native () =
  let runner (nm, dn, f) =
    nm >::
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let t =
         setup_test_directories test_ctxt ~is_native
           ~native_dynlink:(is_native && (native_dynlink test_ctxt))
           (in_testdata_dir test_ctxt ["TestFull"; dn])
       in
         f test_ctxt t)
  in
    List.map runner
      (List.flatten
         [
           List.map (fun (a, b) -> a, a, b) all_tests;
           different_directory_tests;
         ])


let tests =
  "TestFull" >:::
  [
    "all_TestFull" >::
    (fun test_ctxt ->
       all_subdirectories test_ctxt
         (in_testdata_dir test_ctxt ["TestFull"])
         ((List.map fst all_tests)
         @ (List.map (fun (_, a, _) -> a) different_directory_tests))
         (Printf.sprintf "test/data/TestFull/%s is not tested."));

    "best=native" >:::
    (skip_test_on_non_native_arch
       (gen_tests ~is_native:true ()));

    "best=byte" >:::
    (gen_tests ~is_native:false ());
  ]
