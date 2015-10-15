
open OUnit2
open OASISSourcePatterns

let tests =
  "OASISSourcePatterns" >::
  (fun test_ctxt ->
     let exp = "abcd.ml" in
     let modul = "Abcd" in
     let fs_lst = ["abcd.ml"] in
     let fs = new TestCommon.spy_fs test_ctxt (new TestCommon.mem_fs) in

     (* Populate filesystem *)
     List.iter
       (fun fn ->
          OASISFileSystem.defer_close
            (fs#open_out (OASISFileSystem.of_unix_filename fn))
            ignore)
       fs_lst;
     try
       assert_equal
         ~printer:(Printf.sprintf "%S")
         exp
         (List.find
            (fun ufn -> fs#file_exists (OASISFileSystem.of_unix_filename ufn))
            (all_possible_files
               implementation
               ~path:OASISUnixPath.current_dir_name
               ~modul:"Abcd"))
     with Not_found ->
       assert_failure
         (Printf.sprintf
            "Unable to find module %S among files: %s"
            modul (String.concat ", " (List.map (Printf.sprintf "%S") fs_lst))))
