
open OUnit2
open TestCommon
open TestFullUtils

let test_one ?dev ?dynamic ?nocompat ?setup_ml_org test_ctxt fn =
  let t =
    setup_test_directories
      test_ctxt
      ~is_native:(is_native test_ctxt)
      ~native_dynlink:(native_dynlink test_ctxt)
      (in_testdata_dir test_ctxt ["TestBaseCompat"])
  in
  begin
    match setup_ml_org with
    | Some fn -> FileUtil.cp [in_src_dir t fn] (in_src_dir t setup_ml)
    | None -> ()
  end;
  FileUtil.cp [in_src_dir t fn] (in_src_dir t OASISParse.default_oasis_fn);
  oasis_setup ?dev ?dynamic ?nocompat test_ctxt t;
  (in_src_dir t setup_ml), t


let assert_contains fn what =
  assert_bool
    (Printf.sprintf "File %S should contain %S." fn what)
    (OASISString.contains ~what (file_content fn))


let assert_doesnt_contain fn what =
  assert_bool
    (Printf.sprintf "File %S shouldn't contain %S." fn what)
    (not (OASISString.contains ~what (file_content fn)))


let assert_present fn =
  assert_bool
    (Printf.sprintf "File %s should be present." fn)
    (Sys.file_exists fn)


let assert_absent fn =
  assert_bool
    (Printf.sprintf "File %s should not be present." fn)
    (not (Sys.file_exists fn))


let tests =
  "BaseCompat" >:::
  [
    "oasis-0.3" >::
    (fun test_ctxt ->
       let setup_ml, _ = test_one test_ctxt "_oasis-0.3" in
       assert_contains setup_ml "open BaseCompat.Compat_0_3";
       assert_doesnt_contain setup_ml "open BaseCompat.Compat_0_4");

    "oasis-0.4" >::
    (fun test_ctxt ->
       let setup_ml, t =
         test_one test_ctxt ~setup_ml_org:"setup-0.4.ml" "_oasis-0.4"
       in
       let test_fn = in_src_dir t "foobar" in
       assert_doesnt_contain setup_ml "open BaseCompat.Compat_0_3";
       assert_contains setup_ml "open BaseCompat.Compat_0_4";
       run_ocaml_setup_ml test_ctxt t ["-version"];
       assert_absent test_fn;
       run_ocaml_setup_ml test_ctxt t ["-configure"];
       assert_present test_fn;
       run_ocaml_setup_ml test_ctxt t ["-distclean"];
       assert_absent test_fn);

    "oasis-nocompat" >::
    (fun test_ctxt ->
       let setup_ml, _ = test_one ~nocompat:true test_ctxt "_oasis-0.4" in
       assert_doesnt_contain setup_ml "open BaseCompat.Compat_0_3";
       assert_doesnt_contain setup_ml "open BaseCompat.Compat_0_4");

    "oasis-dynrun" >::
    (fun test_ctxt ->
       let setup_ml, t =
         test_one
           ~setup_ml_org:"setup-0.4.ml"
           ~dev:true
           ~dynamic:true
           test_ctxt
           "_oasis-0.4"
       in
       let test_fn = in_src_dir t "foobar" in
       dbug_file_content test_ctxt setup_ml;
       assert_absent test_fn;
       run_ocaml_setup_ml test_ctxt t ["-configure"];
       assert_present test_fn);
  ]
