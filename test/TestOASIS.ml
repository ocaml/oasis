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


(** Tests for OASIS
    @author Sylvain Le Gall
  *)


open OUnit2
open TestCommon
open OASISTypes
open OASISParse
open OASISRecDescParser
open OASISValues
open OASISVersion
open OASISExpr
open FileUtil


let tests =

  (* Check flag equality *)
  let assert_flag nm pkg =
    try
      let _ =
        List.find
          (function
             | Flag (cs, _) -> cs.cs_name = nm
             | _ -> false)
          pkg.sections
      in
        ()
    with Not_found ->
      assert_failure
        (Printf.sprintf
           "No flag '%s' defined"
           nm)
  in

  (* Check that at least one alternative doesn't raise an exception *)
  let assert_alternative msg lst e =
    let found_one =
      List.fold_left
        (fun r t ->
           if not r then
             (
               try
                 t e; true
               with _ ->
                 false
             )
           else
             r)
        false
        lst
    in
      if not found_one then
        assert_failure msg
  in

  let check_one (fn, test) =
     let pkg = from_file ~ctxt:oasis_ignore_plugin_ctxt fn in
     test pkg
  in

  let test_file_of_vector (fn, test) =
    fn >::
    (fun test_ctxt ->
       check_one (in_testdata_dir test_ctxt ["TestOASIS"; fn], test))
  in

  let test_value_parser_of_vector (str, value_parse, fail) =
    str >::
    (fun test_ctxt ->
       try
         (
           let _s: comparator =
             value_parse str
           in
             if fail then
               assert_failure
                 (Printf.sprintf "Parsing '%s' should have failed" str)
         )
       with _ ->
         (
           if not fail then
             assert_failure
               (Printf.sprintf "Parsing '%s' should not have failed" str)
         ))
  in

  let printer_optional_string =
    function
      | Some str -> Printf.sprintf "%S" str
      | None -> "<none>"
  in

  let printer_description =
    function
      | Some txt -> OASISText.to_string txt
      | None -> "<none>"
  in

    "OASIS" >:::
    [
      "ValueParser" >:::
      (List.map test_value_parser_of_vector
         (List.map
            (fun (v, f) ->
               (v,
                OASISVersion.comparator_value.parse
                  ~ctxt:oasis_ctxt,
                f))
            [
              ">= 3.11.1", false;
              ">= 3.11",   false;
              "<= 3.11.1", false;
              "> 3.11.1",  false;
              "< 3.11.1",  false;
              "= 3.11.1",  false;
              ">= 3.11 && <= 3.12", false;
              "= 3.11 || = 3.12", false;
              "= || = 3.12", true;
            ])
      );

      "File" >:::
      (List.map test_file_of_vector
         [
           "test1.oasis",
           (fun pkg ->
              assert_flag "devmod" pkg;
              assert_alternative
                "At least one of ostest, linuxtest64 and linuxtest32 is defined"
                (List.map
                   (fun nm -> (fun () -> assert_flag nm pkg))
                   [
                     "ostest";
                     "linuxtest64";
                     "linuxtest32";
                   ])
                ());

           "test2.oasis",
           ignore;

           "test3.oasis",
           ignore;

           "test4.oasis",
           ignore;

           "test5.oasis",
           ignore;

           "test6.oasis",
           ignore;

           "test7.oasis",
           ignore;

           "test8.oasis",
           ignore;

           "test9.oasis",
           (fun pkg ->
              let deps =
                List.fold_left
                  (fun acc ->
                     function
                       | Executable (cs, bs, _) ->
                           if cs.cs_name = "test" then
                             bs.bs_build_depends @ acc
                           else
                             acc
                       | _ ->
                           acc)
                  []
                  pkg.sections
              in
                List.iter
                  (fun lib ->
                     assert_bool
                       (Printf.sprintf
                          "Existence of library %s"
                          (match lib with
                             | InternalLibrary s -> s
                             | FindlibPackage (s, _) -> s))
                       (List.mem
                          lib
                          deps))
                  ((List.map
                      (fun s -> FindlibPackage(s, None))
                      ["test1"; "pa_test1"; "test_with_str"])
                   @
                   (List.map
                      (fun s -> InternalLibrary s)
                      ["test1"; "pa_test1"; "test_with_str"])));

           "test10.oasis",
           (fun pkg ->
              let flag_test =
                match OASISSection.section_find
                        (`Flag, "test")
                        pkg.sections with
                  | Flag (_, e) -> e
                  | _ -> assert false
              in
              let test_main =
                match OASISSection.section_find
                        (`Test, "main")
                        pkg.sections with
                  | Test (_, e)-> e
                  | _ -> assert false
              in
              let choose_with_env ?(vars=[]) v =
                OASISExpr.choose
                  (fun nm ->
                     try
                       List.assoc nm vars
                     with Not_found ->
                       failwith ("Unable to find var "^nm))
                  v
              in
                assert_equal
                  ~msg:"Default for flag 'test' when os_type='win32'"
                  ~printer:string_of_bool
                  true
                  (choose_with_env
                     ~vars:["os_type", "win32"]
                     flag_test.flag_default);

                assert_equal
                  ~msg:"Default for flag 'test' when os_type='linux'"
                  ~printer:string_of_bool
                  false
                  (choose_with_env
                     ~vars:["os_type", "linux"]
                     flag_test.flag_default);

               assert_equal
                 ~msg:"Default for authors"
                 ~printer:(String.concat ", ")
                 ["Sylvain Le Gall"; "Another one"]
                 pkg.authors;

               assert_equal
                 ~msg:"Synopsis"
                 ~printer:(fun s -> s)
                 "Just a test with extra text"
                 pkg.synopsis;

               assert_equal
                 ~msg:"Command of test 'main' with test='true'"
                 ~printer:(fun (cmd, args) -> String.concat " " (cmd :: args))
                 ("main", ["-test"])
                 (choose_with_env
                    ~vars:["test", "true"]
                    test_main.test_command);

               assert_equal
                 ~msg:"Command of test 'main' with test='false'"
                 ~printer:(fun (cmd, args) -> String.concat " " (cmd :: args))
                 ("main", [])
                 (choose_with_env
                    ~vars:["test", "false"]
                    test_main.test_command);
           );

           "comment-in-field.oasis",
           ignore;

           "bug571.oasis",
           ignore;

           "test-freeform.oasis",
           (fun pkg ->
              assert_equal
                ~printer:printer_description
                (Some (OASISText.of_string "a\nb\n\nc"))
                pkg.description);

           "test11.oasis",
           ignore;

           "test12.oasis",
           (fun pkg ->
              assert_equal
                ~printer:printer_description
                (Some
                   (OASISText.of_string
                      "Thin bindings to various low-level system APIs \
                       (often non-portable)\n\
                       which are not covered by Unix module.\n\
                       \n\
                       Example functions:\n\
                       \ * uname\n\
                       \ * statvfs\n\
                       \ * fsync"))
                pkg.description;
              assert_equal
                ~printer:printer_optional_string
                (Some
                   "Foo is a great library for:\n\
                    \ * pattern matching\n\
                    \ * GC")
                (match OASISSection.section_find
                         (`Doc, "foo")
                         pkg.sections with
                   | Doc (_, doc) ->
                       doc.doc_abstract
                   | _ ->
                       assert false));

           "test14.oasis",
           (fun pkg ->
              let _, _, lib_name_of_findlib_name =
                OASISFindlib.findlib_mapping pkg
              in
                List.iter
                  (fun (fndlb_nm, lib_nm) ->
                     assert_equal
                       ~msg:(Printf.sprintf
                               "library name of findlib package %s"
                               fndlb_nm)
                       ~printer:(fun s -> s)
                       lib_nm
                       (lib_name_of_findlib_name fndlb_nm))
                  ["test", "test1";
                   "test.test2", "test2";
                   "test.test2.test3", "test3";
                   "test.test2.test3.test4.test5p", "test5";
                   "test.test2.test3.test4.test5p.test6", "test6"]);

           "test16.oasis",
           ignore;

           "bug1239.oasis",
           (fun pkg ->
              let template_by_fn l fn =
                try
                  Some (List.find (fun t -> t.OASISFileTemplate.fn = fn) l)
                with Not_found ->
                  None
              in
              let template_body x =
                match x.OASISFileTemplate.body with
                  | OASISFileTemplate.NoBody -> []
                  | OASISFileTemplate.Body l -> l
                  | OASISFileTemplate.BodyWithDigest (_, l) -> l
              in
              let initial_ctxt = {
                OASISPlugin.ctxt = OASISContext.quiet ;
                update = OASISSetupUpdate.NoUpdate;
                error = false ;
                files = OASISFileTemplate.create ~disable_oasis_section:[] ();
                other_actions = []
              }
              in
              let ctxt =
                OCamlbuildPlugin.add_ocamlbuild_files initial_ctxt pkg
              in
              let templates =
                OASISFileTemplate.fold
                  (fun t accu -> t :: accu)
                  ctxt.OASISPlugin.files []
              in
              let mllib =
                match template_by_fn templates "src/bar/bar.mllib" with
                  | None ->
                      let msg =
                        Printf.sprintf
                          "Missing mllib file for packed library bar, \
                           here is the list of generated files:\n%s\n"
                          (String.concat "\n"
                             (List.map
                                (fun t -> t.OASISFileTemplate.fn)
                                templates))
                      in
                        assert_failure msg
                  | Some x -> x
              in
                assert_equal
                  ~msg:"The mllib of a packed library should contain the name \
                        of the pack"
                  ~printer:(fun x ->
                              Printf.sprintf "[ %s ]"
                                (String.concat " ; "
                                   (List.map (Printf.sprintf "%S") x)))
                  [ "Bar" ]
                  (template_body mllib)
           );
         ])
      @
      [
        "Examples" >::
        (fun test_ctxt ->
           let lst_examples =
             (find
                (* Collect _oasis in examples/ *)
                (Basename_is "_oasis")
                (example_dir test_ctxt)
                (fun a e -> e :: a)
                [])
             @
             (filter
                (* Collect examples/oasis/*.oasis *)
                (Has_extension "oasis")
                (ls (example_dir test_ctxt)))
           in
             List.iter (fun fn -> check_one (fn, ignore)) lst_examples);

        "SinceVersion" >::
        (fun test_ctxt ->
           assert_raises
             ~msg:"Pack is supported only in 0.3"
             (Failure "Field Pack in Library test1 is only valid since \
                       OASIS v0.3, update OASISFormat field from \
                       '0.2' to '0.3' after checking OASIS changelog.")
             (fun () ->
                let _pkg =
                  from_file
                    ~ctxt:oasis_ignore_plugin_ctxt
                    (in_testdata_dir test_ctxt ["TestOASIS"; "test13.oasis"])
                in
                  ()));

        "test15.oasis" >::
        (fun test_ctxt ->
           try
             let _pkg: OASISTypes.package =
                from_file
                  ~ctxt:oasis_ignore_plugin_ctxt
                  (in_testdata_dir test_ctxt ["TestOASIS"; "test13.oasis"])
              in
               assert_string "test15.oasis should fail to parse"
          with Failure _ ->
            ());

       "bug1236.oasis" >::
       (fun test_ctxt ->
          assert_raises
            ~msg:"Not allowed to use lowercase module name."
            (Failure "Module name 'lib', must be capitalized ('Lib').")
            (fun () ->
              let _pkg =
                from_file
                  ~ctxt:oasis_ignore_plugin_ctxt
                  (in_testdata_dir test_ctxt ["TestOASIS"; "bug1236.oasis"])
              in
                ()));

       "bug1295.oasis" >::
       (fun test_ctxt ->
          assert_raises
            ~msg:"Unexpected indentation line 17."
            (Failure "Unexpected indentation line 17.")
            (fun () ->
              let _pkg =
                from_file
                  ~ctxt:oasis_ignore_plugin_ctxt
                  (in_testdata_dir test_ctxt ["TestOASIS"; "bug1295.oasis"])
              in
                ()));
      ]
    ]
