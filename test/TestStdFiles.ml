
(** Test plugin StdFiles
    @author Sylvain Le Gall
 *)

open OUnit2
open TestCommon
open OASISPlugin
open OASISFileTemplate

module Line =
struct
  type t = string
  let compare = String.compare
  let pp_printer fmt = Format.fprintf fmt "%S"
  let pp_print_sep = Format.pp_print_newline
end

module DiffFileContent =
struct
  include OUnitDiff.ListSimpleMake (Line)

  let of_file fn =
    OASISString.split_newline ~do_trim:false
      (TestCommon.file_content fn)
end


let file_match_expectation test_ctxt ctxt dn fn =
  let digest_filter_out =
    List.filter
      (fun line ->
         not (OASISString.contains ~what:"DO NOT EDIT (digest:" line))
  in
  let tmpl = to_string_list (find fn ctxt.files) in
    logf test_ctxt `Info "File generated %s" fn;
    List.iter (logf test_ctxt `Info "%s") tmpl;
    DiffFileContent.assert_equal
      ~msg:"README.md"
      (digest_filter_out
         (DiffFileContent.of_file (Filename.concat dn (fn ^ ".exp"))))
      (digest_filter_out
         (DiffFileContent.of_list (tmpl @ [""] (* eol at eof *))))


let tests =
  "TestStdFiles" >:::
  [
    "markdown" >::
    (fun test_ctxt ->
       (* Parse string to get OASIS package *)
       let dn = in_testdata_dir test_ctxt ["TestStdFiles"; "oasis"] in
       let pkg =
         OASISParse.from_file ~ctxt:oasis_ctxt
           (Filename.concat dn OASISParse.default_oasis_fn)
       in
       let ctxt, _ =
         with_bracket_chdir test_ctxt dn
           (fun test_ctxt ->
              BaseSetup.of_package ~setup_update:false
                OASISSetupUpdate.NoUpdate pkg)
       in
         file_match_expectation test_ctxt ctxt dn "README.md";
         file_match_expectation test_ctxt ctxt dn "INSTALL.md";
         file_match_expectation test_ctxt ctxt dn "AUTHORS.md")
  ]
