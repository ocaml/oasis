
open OUnit2
open TestCommon
open OASISTypes

let tests =
  "OASISObject" >::
  (fun test_ctxt ->
     let in_dir lst =
       in_testdata_dir test_ctxt
         (["TestOASISObject"; "source_patterns"] @ lst)
     in
     let ctxt = oasis_ctxt test_ctxt in
     let sfs = new OASISFileSystem.host_fs (in_dir []) in
     let pkg = OASISParse.from_file ~ctxt (in_dir ["_oasis"]) in
     let lst =
       List.fold_left
         (fun acc sct ->
            match sct with
            | Object (cs, bs, obj) ->
              let lst =
                OASISObject.source_unix_files
                  ~ctxt
                  (cs, bs, obj)
                  (fun fn ->
                     sfs#file_exists (OASISFileSystem.of_unix_filename fn))
              in
              (List.flatten (List.rev_map snd lst)) @ acc
            | Library _ | Executable _ | Flag _ | SrcRepo _ | Test _ | Doc _ ->
              acc)
         [] pkg.sections
     in
     DiffSetOutput.assert_equal
       (DiffSetOutput.of_list ["O1.eliom"; "O1.eliomi"; "O2.ml"])
       (DiffSetOutput.of_list lst))
