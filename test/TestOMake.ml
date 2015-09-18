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

let oasis_omake_files dirs =
  let files =
    ["OMakefile"; "_oasis_build.om"; "_oasis_hier.om"; "_oasis_install.om"]
  in
    List.flatten
      (("OMakeroot" :: "_oasis_lib.om" :: files) ::
       (List.map (fun dn -> List.map (fun bn -> Filename.concat dn bn) files)
          dirs))


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
         register_generated_files t
           (oasis_omake_files ["src"]);
         register_installed_files test_ctxt t
           [
             InstalledOCamlLibrary
               ("simplelib",
                ["META";
                 "bar.mli"; "bar.cmi"; "bar.cmx";
                 (* TODO: "bar.annot"; "bar.cmt" *)
                 "foo.mli"; "foo.cmi"; "foo.cmx";
                 (* TODO: "foo.annot"; "foo.cmt"; *)
                 "simplelib.cma"; "simplelib.cmxa"; "simplelib.cmxs";
                 "simplelib.a"]);
             InstalledAPIRef("simplelib", ["Foo"; "Bar"]);
           ];
         (* Run standard test. *)
         standard_test test_ctxt t;
         (* Try the result. *)
         try_installed_library test_ctxt t "simplelib" ["Foo"];
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
         register_generated_files t
           (oasis_omake_files
              ["src"; "src/liba"; "src/libb"; "src/libc"; "src/libwithc";
               "src/exec"; "src/packedlib"]);
         register_generated_files t
           ["src/liba/META"; "src/libb/META"; "src/libc/META";
            "src/libwithc/META"; "src/packedlib/META"];
         register_installed_files test_ctxt t
           [
             InstalledBin ["exec"];
             InstalledOCamlLibrary
               ("liba",
                ["META"; "a1.cmx"; "a2.cmi"; "a2.cmx"; "a2.ml"; "liba.a";
                 "liba.cma"; "liba.cmxa"; "liba.cmxs"]);
             InstalledOCamlLibrary
               ("libb",
                ["META"; "B1.cmi"; "B1.cmx"; "B1.ml"; "libb.a"; "libb.cma";
                 "libb.cmxa"; "libb.cmxs"]);
             InstalledOCamlLibrary
               ("libc",
                ["META"; "c1.cmi"; "c1.cmx"; "c1.ml"; "libc.a"; "libc.cma";
                 "libc.cmxa"; "libc.cmxs"]);
             InstalledOCamlLibrary
               ("libwithc",
                ["META"; "dlllibwithc_stubs.so"; "liblibwithc_stubs.a";
                 "libwithc.a"; "libwithc.cma"; "libwithc.cmxa";
                 "libwithc.cmxs"; "p.cmi"; "p.cmx"; "p.ml"]);
             InstalledOCamlLibrary
               ("packedlib",
                ["META"; "packedlib.a"; "packedlib.cma"; "packedlib.cmi";
                 "packedlib.cmt"; "packedlib.cmx"; "packedlib.cmxa";
                 "packedlib.cmxs"; "q.cmt"; "q.ml"]);
             InstalledAPIRef("interdepend", ["C1"]);
           ];
         (* Run standard test. *)
         standard_test test_ctxt t;
         (* Try the result. *)
         try_installed_library test_ctxt t "liba" ["A2"];
         try_installed_library test_ctxt t "libb" ["B1"];
      (* TODO: should work.
         try_installed_library test_ctxt t "libc" ["C1"];
       *)
         try_installed_library test_ctxt t "libwithc" ["P"];
         try_installed_library test_ctxt t "packedlib" ["Packedlib"];
    );
  ]
