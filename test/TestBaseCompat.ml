
open OUnit2
open TestCommon

let test_one ?(setup_options=[]) ?setup_ml_org test_ctxt fn =
  let tmpdir = bracket_tmpdir test_ctxt in
  let in_testdata_dir fn = in_testdata_dir test_ctxt ["TestBaseCompat"; fn] in
  let setup_ml = Filename.concat tmpdir BaseSetup.default_filename in
  FileUtil.cp [in_testdata_dir "A.ml"] tmpdir;
  begin
    match setup_ml_org with
    | Some fn -> FileUtil.cp [in_testdata_dir fn] setup_ml
    | None -> ()
  end;
  FileUtil.cp
    [in_testdata_dir fn]
    (Filename.concat tmpdir OASISParse.default_oasis_fn);
  assert_oasis_cli
    ~ctxt:test_ctxt
    ~chdir:tmpdir
    (["setup"; "-real-oasis"] @ setup_options);
  setup_ml

let assert_contains fn what =
  assert_bool
    (Printf.sprintf "File %S should contain %S." fn what)
    (OASISString.contains ~what (file_content fn))

let assert_doesnt_contain fn what =
  assert_bool
    (Printf.sprintf "File %S shouldn't contain %S." fn what)
    (not (OASISString.contains ~what (file_content fn)))

let tests =
  "BaseCompat" >:::
  [
    "oasis-0.3" >::
    (fun test_ctxt ->
       let setup_ml = test_one test_ctxt "_oasis-0.3" in
       assert_contains setup_ml "open BaseCompat.Compat_0_3";
       assert_doesnt_contain setup_ml "open BaseCompat.Compat_0_4");

    "oasis-0.4" >::
    (fun test_ctxt ->
       let setup_ml =
         test_one test_ctxt ~setup_ml_org:"setup-0.4.ml" "_oasis-0.4"
       in
       let srcdir = Filename.dirname setup_ml in
       let test_fn = Filename.concat srcdir "foobar" in
       let assert_present fn =
         assert_bool
           (Printf.sprintf "File %s should be present." fn)
           (Sys.file_exists fn)
       in
       let assert_absent fn =
         assert_bool
           (Printf.sprintf "File %s should not be present." fn)
           (not (Sys.file_exists fn))
       in
       let assert_ocaml_setup_ml args =
         assert_command ~ctxt:test_ctxt ~chdir:srcdir "ocaml" (setup_ml :: args)
       in
       assert_doesnt_contain setup_ml "open BaseCompat.Compat_0_3";
       assert_contains setup_ml "open BaseCompat.Compat_0_4";
       assert_ocaml_setup_ml ["-version"];
       assert_absent test_fn;
       assert_ocaml_setup_ml ["-configure"];
       assert_present test_fn;
       assert_ocaml_setup_ml ["-distclean"];
       assert_absent test_fn);

    "oasis-no-compat" >::
    (fun test_ctxt ->
       let setup_ml =
         test_one ~setup_options:["-no-compat"] test_ctxt "_oasis-0.4"
       in
       assert_doesnt_contain setup_ml "open BaseCompat.Compat_0_3";
       assert_doesnt_contain setup_ml "open BaseCompat.Compat_0_4");
  ]
