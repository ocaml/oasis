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
open TestCommon
open OASISPlugin
open TestFullUtils

let oasis_omake_files dirs =
  let files =
    ["OMakefile"; "_oasis_build.om"; "_oasis_hier.om"; "_oasis_install.om"]
  in
    List.flatten
      (("OMakeroot" :: "_oasis_lib.om" :: files) ::
       (List.map (fun dn -> List.map (fun bn -> Filename.concat dn bn) files)
          dirs))


let all_tests =
  [
    "simplelib",
    (fun test_ctxt t ->
       oasis_setup test_ctxt t;
       register_generated_files t
         (oasis_omake_files ["src"]);
       register_installed_files test_ctxt t
         [
           InstalledOCamlLibrary
             ("simplelib",
              ["META";
               "bar.mli"; "bar.cmi"; "bar.cmx"; "bar.annot"; "bar.cmt"; "bar.cmti";
               "foo.mli"; "foo.cmi"; "foo.cmx"; "foo.annot"; "foo.cmt"; "foo.cmti";
               "simplelib.cma"; "simplelib.cmxa"; "simplelib.cmxs";
               "simplelib.a"]);
           InstalledAPIRef("simplelib", ["Foo"; "Bar"]);
         ];
       (* Run standard test. *)
       standard_test test_ctxt t;
       (* Try the result. *)
       try_installed_library test_ctxt t "simplelib" ["Foo"];
    );

    "complex",
    (fun test_ctxt t ->
       oasis_setup test_ctxt t;
       register_generated_files t
         (oasis_omake_files
            ["src"; "src/liba"; "src/libb"; "src/libc_"; "src/libwithc";
             "src/exec"; "src/packedlib"]);
       register_generated_files t
         ["src/liba/META"; "src/libb/META"; "src/libc_/META";
          "src/libwithc/META"; "src/packedlib/META"];
       register_installed_files test_ctxt t
         [
           InstalledBin ["exec"];
           InstalledOCamlLibrary
             ("liba",
              ["META"; "a1.cmx"; "a2.cmi"; "a2.cmx"; "a2.ml"; "a2.cmt";
               "a2.annot"; "liba.a"; "liba.cma"; "liba.cmxa"; "liba.cmxs"]);
           InstalledOCamlLibrary
             ("libb",
              ["META"; "B1.cmi"; "B1.cmx"; "B1.ml"; "B1.cmt"; "B1.annot";
               "libb.a"; "libb.cma"; "libb.cmxa"; "libb.cmxs"]);
           InstalledOCamlLibrary
             ("libc_",
              ["META"; "c1.cmi"; "c1.cmx"; "c1.mli"; "c1.cmti"; "c1.cmt";
               "c1.annot"; "libc_.a"; "libc_.cma"; "libc_.cmxa";
               "libc_.cmxs"]);
           InstalledOCamlLibrary
             ("libwithc",
              ["META"; "dlllibwithc_stubs.so"; "liblibwithc_stubs.a";
               "libwithc.a"; "libwithc.cma"; "libwithc.cmxa"; "libwithc.cmxs";
               "p.cmi"; "p.cmx"; "p.ml"; "p.cmt"; "p.annot"]);
           InstalledOCamlLibrary
             ("packedlib",
              ["META"; "packedlib.a"; "packedlib.cma"; "packedlib.cmi";
               "packedlib.cmt"; "packedlib.cmx"; "packedlib.cmxa";
               "packedlib.cmxs"; "q.annot"; "q.cmt"; "q.ml"]);
           InstalledAPIRef("interdepend", ["C1"]);
         ];
       (* Run standard test. *)
       standard_test test_ctxt t;
       (* Try the result. *)
       try_installed_library test_ctxt t "liba" ["A2"];
       try_installed_library test_ctxt t "libb" ["B1"];
       try_installed_library test_ctxt t "libc_" ["C1"];
       try_installed_library test_ctxt t "libwithc" ["P"];
       try_installed_library test_ctxt t "packedlib" ["Packedlib"];
    );
  ]

let gen_test (nm, f) =
  nm >::
  (fun test_ctxt ->
     let t =
       setup_test_directories test_ctxt
         ~is_native:(is_native test_ctxt)
         ~native_dynlink:(native_dynlink test_ctxt)
         (in_testdata_dir test_ctxt ["TestPluginOMake"; nm])
     in
       f test_ctxt t)

let tests =
  "Plugin OMake" >:::
  List.flatten
    [
      [
        "generation" >::
        (fun test_ctxt ->
           let generate ?(want_error=false) bn =
             let dn = in_testdata_dir test_ctxt ["TestPluginOMake"] in
             let fn = in_testdata_dir test_ctxt ("TestPluginOMake" :: bn) in
             let pkg =
               logf test_ctxt `Info "Parsing file %S." fn;
               OASISParse.from_file
                 ~ctxt:(oasis_ctxt ~ignore_plugin:true test_ctxt)
                 fn
             in
             let ctxt, _ =
               logf test_ctxt `Info "Generating setup using file %S." fn;
               with_bracket_chdir test_ctxt dn
                 (fun _ ->
                    BaseSetup.of_package
                      ~ctxt:(oasis_ctxt test_ctxt)
                      ~setup_update:false
                      OASISSetupUpdate.NoUpdate
                      pkg)
             in
             if want_error && not ctxt.error then
               assert_failure "Expecting an error during generation, got none."
             else if not want_error && ctxt.error then
               assert_failure "Expecting no error during generation, got one."
           in
           generate ["simplelib"; "_oasis"];
           generate ~want_error:true ["noocamlversion.oasis"];
           generate ~want_error:true ["ocamlversion312.oasis"];
           generate ["ocamlversion401.oasis"];
           generate ["ocamlversion402.oasis"]);

        "all_TestPluginOMake" >::
        (fun test_ctxt ->
           all_subdirectories test_ctxt
             (in_testdata_dir test_ctxt ["TestPluginOMake"])
             (List.rev_append
                ["noocamlversion.oasis"; "ocamlversion312.oasis";
                 "ocamlversion401.oasis"; "ocamlversion402.oasis"]
                (List.map fst all_tests))
             (Printf.sprintf "test/data/TestOCamlbuild/%s is not tested."));
      ];
      List.map gen_test all_tests;
  ]
