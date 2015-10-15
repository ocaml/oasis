open OUnit2
open TestCommon
open OASISTypes

let tests =
  "OASISLibrary" >::
  (fun test_ctxt ->
     let in_dir lst =
       in_testdata_dir test_ctxt
         (["TestOASISLibrary"; "source_patterns"] @ lst)
     in
     let ctxt = oasis_ctxt test_ctxt in
     let sfs = new OASISFileSystem.host_fs (in_dir []) in
     let pkg = OASISParse.from_file ~ctxt (in_dir ["_oasis"]) in
     let lst =
       List.fold_left
         (fun acc sct ->
            match sct with
            | Library (cs, bs, lib) ->
              let lst =
                OASISLibrary.source_unix_files
                  ~ctxt
                  (cs, bs, lib)
                  (fun fn ->
                     sfs#file_exists (OASISFileSystem.of_unix_filename fn))
              in
              (List.flatten (List.rev_map snd lst)) @ acc
            | Object _ | Executable _ | Flag _ | SrcRepo _ | Test _ | Doc _ ->
              acc)
         [] pkg.sections
     in
     DiffSetOutput.assert_equal
       (DiffSetOutput.of_list ["L1.ml"; "L2.mli"; "L3.mlify"])
       (DiffSetOutput.of_list lst))
