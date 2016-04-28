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


let tests =
  "Plugin OCamlbuild" >:::
  [
    "missing-source" >::
    (fun test_ctxt ->
       let dn =
         in_testdata_dir test_ctxt ["TestOCamlbuild"; "missing-source"]
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
       let tmpl = find "test.mllib" ctxt.files in
         match tmpl.body with
           | Body lst | BodyWithDigest (_, lst) ->
               assert_equal
                 ~printer:(fun lst ->
                             String.concat ", "
                               (List.map (Printf.sprintf "%S") lst))
                 ["A"; "B"; "C"]
                 (List.sort String.compare lst);
           | NoBody ->
               assert_failure "No content for test.mllib.");

    "set-ocamlfind" >::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt
           ~is_native:(is_native test_ctxt)
           ~native_dynlink:(native_dynlink test_ctxt)
           (in_testdata_dir test_ctxt ["TestOCamlbuild"; "set-ocamlfind"])
       in
       let () =
         skip_if
           (OASISVersion.version_compare_string t.ocaml_version "3.12.1" < 0)
           "OCaml >= 3.12.1 needed."
       in
       let real_ocamlfind = FileUtil.which "ocamlfind" in
       let fake_ocamlfind =
         Filename.concat t.bin_dir (Filename.basename real_ocamlfind)
       in
       let extra_env = ["REAL_OCAMLFIND", real_ocamlfind] in
       let () =
         oasis_setup test_ctxt t;
         FileUtil.cp [fake_ocamlfind_exec test_ctxt] fake_ocamlfind;
         Unix.chmod fake_ocamlfind 0o755;
         run_ocaml_setup_ml ~with_ocaml_env:true ~extra_env test_ctxt t
           ["-configure"]
       in
       let env = BaseEnvLight.load ~filename:(in_src_dir t "setup.data") () in
       let () =
         assert_equal ~printer:(Printf.sprintf "%S")
           fake_ocamlfind
           (BaseEnvLight.var_get "ocamlfind" env);
         run_ocaml_setup_ml ~extra_env test_ctxt t ["-build"]
       in
       let build_log =
         file_content (in_src_dir t (Filename.concat "_build" "_log"))
       in
         logf test_ctxt `Info "%s" build_log;
         List.iter
           (fun line ->
              if OASISString.contains ~what:"ocamlfind" line then
                assert_bool
                  (Printf.sprintf
                     "line %S should starts with %S"
                     line fake_ocamlfind)
                  (OASISString.starts_with ~what:fake_ocamlfind line))
           (OASISString.nsplit build_log '\n'));

    "use-ocamlfind" >::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt
           ~is_native:(is_native test_ctxt)
           ~native_dynlink:(native_dynlink test_ctxt)
           (in_testdata_dir test_ctxt ["TestOCamlbuild"; "use-ocamlfind"])
       in
       oasis_setup test_ctxt t;
       run_ocaml_setup_ml ~check_output:true test_ctxt t
         ["-configure"; "--enable-docs"];
       run_ocaml_setup_ml ~check_output:true test_ctxt t
         ["-build"];
       run_ocaml_setup_ml ~check_output:true test_ctxt t
         ["-doc"]);

   (* TODO: reactivate *)
(*
    (* this test changes a c-source file and asserts that an executable
       depending on a library which uses this c-file is re-linked 
       properly 
       TODO: reassert that this test fails without the fix in cb96135a
     *)
    "external source rebuild" >::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt
           ~is_native:(is_native test_ctxt)
           ~native_dynlink:(native_dynlink test_ctxt)
           (in_testdata_dir test_ctxt ["TestOCamlbuild"; "external-c-rebuild"])
	   
       (* contain c-source code here to avoid any problems with
          aborted evaluation *) 
       and code_a = "#include \"header.h\"
                     CAMLprim value oasis_c_build_test_foo(value x) {
                       CAMLparam1(x);
                       CAMLreturn(Val_int(42));
                     }"

       and code_b = "#include \"header.h\"
                     CAMLprim value oasis_c_build_test_foo(value x) {
                       CAMLparam1(x);
                       CAMLreturn(Val_int(23));
                     }"
       in
       let c_source = (in_src_dir t "c_source.c") in

       oasis_setup test_ctxt t;

       ( let c = open_out c_source in Printf.fprintf c "%s\n" code_a ; close_out c );
				    
       run_ocaml_setup_ml ~check_output:true test_ctxt t ["-configure" ];
       run_ocaml_setup_ml ~check_output:true test_ctxt t ["-build"];

       (* first, compile and assert everything worked *)
       assert_bool "File 'B.native' has been created"
		   (Sys.file_exists (in_src_dir t "B.native"));

       assert_command ~ctxt:test_ctxt ~chdir:t.src_dir ~exit_code:(Unix.WEXITED 42) (in_src_dir t "B.native") [];
    
       (* change c-file, rebuild and assert result-code *)
       ( let c = open_out c_source in Printf.fprintf c "%s\n" code_b ; close_out c ) ;

       (* uncomment to make this test succeed *)
       (* run_ocaml_setup_ml ~check_output:true test_ctxt t ["-clean"]; *)
       run_ocaml_setup_ml ~check_output:true test_ctxt t ["-build"];

       assert_command ~ctxt:test_ctxt ~chdir:t.src_dir ~exit_code:(Unix.WEXITED 23) (in_src_dir t "B.native") []     
    );

 *)
   (* TODO: reactivate *)
(*
    "env-tags" >::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt
           ~is_native:(is_native test_ctxt)
           ~native_dynlink:(native_dynlink test_ctxt)
           (in_testdata_dir test_ctxt ["TestOCamlbuild"; "env-tags"])
       in
       let tests_tag_detected_fn = in_src_dir t "tests-tag-detected" in
       oasis_setup test_ctxt t;

       run_ocaml_setup_ml ~check_output:true test_ctxt t ["-configure"; "--enable-tests"];
       run_ocaml_setup_ml ~check_output:true test_ctxt t ["-build"];
       assert_bool
         "tests-tag-detected should be existed."
         (Sys.file_exists tests_tag_detected_fn);

       rm [tests_tag_detected_fn];
       run_ocaml_setup_ml ~check_output:true test_ctxt t ["-configure"; "--disable-tests"];
       run_ocaml_setup_ml ~check_output:true test_ctxt t ["-build"];
       assert_bool
         "tests-tag-detected should not be existed."
         (not (Sys.file_exists tests_tag_detected_fn)))
 *)
  ]
