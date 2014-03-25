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

(** Test plugin StdFiles
    @author Sylvain Le Gall
 *)

open OUnit2
open TestCommon
open TestFullUtils
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
         file_match_expectation test_ctxt ctxt dn "AUTHORS.md");

    "remove-section" >::
    (fun test_ctxt ->
       let t =
         setup_test_directories test_ctxt
           ~is_native:(is_native test_ctxt)
           ~native_dynlink:(native_dynlink test_ctxt)
           (in_testdata_dir test_ctxt ["TestStdFiles"; "remove"])
       in
       let pristine_files = all_files t.src_dir in
       let expected_files =
         SetFile.remove
           (in_src_dir t "AUTHORS.txt")
           (SetFile.add_list
              pristine_files
              (List.rev_map (in_src_dir t) ["README.txt"; "INSTALL.txt.bak"]))
       in
         FileUtil.cp [in_src_dir t "README.txt.fst"]
           (in_src_dir t "README.txt");
         FileUtil.cp [in_src_dir t "INSTALL.txt.fst"]
           (in_src_dir t "INSTALL.txt");
         FileUtil.cp [in_src_dir t "AUTHORS.txt.fst"]
           (in_src_dir t "AUTHORS.txt");
         assert_oasis_cli ~ctxt:test_ctxt ~chdir:t.src_dir
           ["setup-clean"; "-remove"];
         DiffFileContent.assert_equal
           ~msg:"INSTALL.txt.bak = INSTALL.txt.fst"
           (DiffFileContent.of_file (in_src_dir t "INSTALL.txt.bak"))
           (DiffFileContent.of_file (in_src_dir t "INSTALL.txt.fst"));
         DiffFileContent.assert_equal
           ~msg:"README.txt = README.txt.snd"
           (DiffFileContent.of_file (in_src_dir t "README.txt"))
           (DiffFileContent.of_file (in_src_dir t "README.txt.snd"));
         SetFile.assert_equal ~root:t.src_dir
           expected_files
           (all_files t.src_dir));
  ]
