
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
       try_installed_library test_ctxt t "foobar.object" ["O"]
    );
  ]
