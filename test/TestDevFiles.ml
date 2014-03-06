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


open OUnit2
open FileUtil
open TestCommon
open TestFullUtils

let run_ocaml_setup_ml
      ?exit_code
      ?check_output
      ?(extra_env=[])
      test_ctxt t arg args =
  (* Speed up for testing, compile setup.ml *)
  let timer = timer_start ("run_ocaml_setup_ml_compiled " ^ arg) in
  let cmd, args =
    let args =
      if arg = "setup.data" then
        []
      else
        [Printf.sprintf
           "%sFLAGS=%s"
           (String.uppercase arg)
           (String.concat " " args)
        ]
    in
    "make", (arg :: args)
  in
  assert_command ~ctxt:test_ctxt ?exit_code ?check_output ~extra_env
    ~chdir:t.src_dir cmd args;
  timer_stop test_ctxt timer


let standard_test_compiled test_ctxt t =
  (* Standard checks. *)
  standard_checks test_ctxt t;

  (* Quick test. *)
  run_ocaml_setup_ml test_ctxt t
    "all" ["--"; "--override"; "is_native"; string_of_bool t.is_native];
  check_myocamlbuild_ml test_ctxt t;

  (* Distclean. *)
  run_ocaml_setup_ml test_ctxt t "distclean" [];
  check_generated_files t;

  assert_command ~ctxt:test_ctxt ~chdir:t.src_dir "make" ["setup.exe"];

  register_generated_files t ["setup.exe"];
  check_generated_files t;

  (* Run configure target *)
  run_ocaml_setup_ml test_ctxt t
      "configure" ["--prefix";  t.build_dir; "--docdir";  t.doc_dir;
       "--htmldir"; t.html_dir;
       "--override"; "is_native"; string_of_bool t.is_native];
  assert_bool "File 'setup.data' has been created"
    (Sys.file_exists (in_src_dir t "setup.data"));

  (* Run build target *)
  run_ocaml_setup_ml test_ctxt t "build" [];

  (* Clean *)
  run_ocaml_setup_ml test_ctxt t "clean" [];

  (* Run build target *)
  run_ocaml_setup_ml test_ctxt t "build" [];

  (* Run test target *)
  run_ocaml_setup_ml test_ctxt t "test" [];

  (* Run documentation target *)
  run_ocaml_setup_ml test_ctxt t "doc" [];

  (* 1st install *)
  run_ocaml_setup_ml test_ctxt t "install" [];
  check_installed_files test_ctxt t "1st install";
  run_ocaml_setup_ml test_ctxt t "uninstall" [];
  check_nothing_installed test_ctxt t "1st uninstall";

  (* 2nd install *)
  run_ocaml_setup_ml test_ctxt t "install" [];
  check_installed_files test_ctxt t "2nd install";
  run_ocaml_setup_ml test_ctxt t "uninstall" [];
  check_nothing_installed test_ctxt t "2nd uninstall";

  (* Run install/uninstall target with destdir *)
  if Sys.os_type <> "Win32" then begin
    (* Prepending something at the beginning of a Win32 path
     * doesn't work because it will create a filename like:
     * c:\a\b\c:\c, which is illegal
     * TODO: find a solution for DESTDIR on Win32
     *)
    let destdir = bracket_tmpdir test_ctxt in
    let () =
      List.iter
        (fun lst ->
           mkdir ~parent:true (FilePath.make_filename (destdir :: lst)))
        [
          ["bin"];
          ["lib"; "ocaml"];
          ["share"; "doc"; "html"]
        ]
    in
    let t =
      {t with
           (* This will change OCAMLPATH as well when running setup.ml. *)
           ocaml_lib_dir = FilePath.make_filename [destdir; "lib"; "ocaml"]}
    in
      run_ocaml_setup_ml test_ctxt t
        ~extra_env:["destdir", destdir] "install" [];

      assert_equal
        ~msg:"Same number of files installed with destdir and without."
        ~printer:string_of_int
        (SetFile.cardinal t.installed_files)
        (SetFile.cardinal (all_files destdir))
  end;

  (* 3rd install *)
  run_ocaml_setup_ml test_ctxt t "reinstall" [];
  check_installed_files test_ctxt t "3rd install"
  (* TODO: auto-test installed libraries. *)


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

    "compiled_setup_ml">::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt in_testdata_dir
           ["TestDevFiles"; "compiled_setup_ml"]
       in
       oasis_setup ~dynamic:true test_ctxt t;
       (* Setup expectation. *)
       register_generated_files t oasis_ocamlbuild_files;
       register_generated_files t ["configure"; "Makefile"];
       (* Run standard test. *)
       standard_test_compiled test_ctxt t;
       let makefile_content = file_content (t.src_dir ^ "/Makefile") in
       assert_bool
         "Test the SETUP variable in the Makefile for compiled_setup_ml."
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
