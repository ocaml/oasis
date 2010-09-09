(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2008-2010, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

(** Tests for OASIS
    @author Sylvain Le Gall
  *)

open OUnit
open TestCommon
open OASISTypes
open OASISParse
open OASISRecDescParser
open OASISValues
open OASISVersion
open OASISExpr
open FileUtil

let tests ctxt =

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

  let file_of_vector (fn, test) = 
    fn >::
    (fun () ->
       let pkg =
         from_file 
           ~ctxt:ctxt.oasis_ctxt
           ~ignore_plugins:true
           fn
       in
         test pkg)
  in

  let test_file_of_vector (fn, test) = 
    file_of_vector (in_data fn, test)
  in

  let test_value_parser_of_vector (str, value_parse, fail) = 
    str >::
    (fun () ->
       try
         ( 
           let _s : comparator = 
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

    "OASIS" >:::
    [
      "ValueParser" >:::
      (List.map test_value_parser_of_vector 
         (List.map 
            (fun (v, f) -> 
               (v, 
                OASISVersion.comparator_value.parse 
                  ~ctxt:ctxt.oasis_ctxt, 
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
      ((List.map test_file_of_vector 
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
                         (OASISSection.KFlag, "test") 
                         pkg.sections with
                   | Flag (_, e) -> e
                   | _ -> assert false
               in
               let test_main =
                 match OASISSection.section_find 
                         (OASISSection.KTest, "main") 
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

            "bug571/_oasis",
            ignore;

            "test-freeform.oasis",
            (fun pkg ->
               assert_equal
                 ~printer:(function
                             | Some s -> Printf.sprintf "%S" s
                             | None -> "<none>")
                 (Some "a\nb\n\nc")
                 pkg.description);
          ])
      @
       (List.rev_map file_of_vector
          (List.rev_map 
             (fun fn ->
                fn, ignore)
             (find 
                (* Collect _oasis in examples/ *)
                (Basename_is "_oasis") "../examples" 
                (fun a e -> e :: a) 
                (* Collect examples/oasis/*.oasis *)
                (filter (Has_extension "oasis") 
                   (ls "../examples/oasis"))))));
    ]
;;
