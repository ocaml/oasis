open OUnit2
open TestCommon
open OASISPlugin
open OASISFileTemplate

let tests = 
  "Plugin OCamlbuild" >::
  (fun test_ctxt ->
     let () = skip_if true "Need to work on feature" in
     let dn = in_testdata_dir test_ctxt ["ocamlbuild"; "missing-source"] in
     let fn = Filename.concat dn "_oasis" in
     let pkg = OASISParse.from_file ~ctxt:oasis_ctxt fn in
     let ctxt, _ = 
       with_bracket_chdir test_ctxt dn
         (fun test_ctxt ->
            BaseSetup.of_package ~setup_update:false pkg)
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
             assert_failure "TODO: remove"
         | NoBody ->
             assert_failure "No content for test.mllib.")
