
(** Tests for BaseFileGenerate
    @author Sylvain Le Gall
  *)

open OUnit;;
open TestCommon;;
open BaseFileGenerate;;

let tests ctxt =

  let test_of_vector (fn, content_lst, comment_fmt) = 
    fn >::
    (fun () ->
       let real_fn = 
         in_data fn
       in
       let target_fn =
         Filename.temp_file "filegenerate" ".txt"
       in
       let expected_fn =
         real_fn ^ "-exp"
       in

       let file_content fn =
         let chn =
           open_in_bin fn
         in
         let size =
           in_channel_length chn
         in
         let buff =
           Buffer.create size
         in
           Buffer.add_channel buff chn size;
           close_in chn;
           Buffer.contents buff
       in

       let verbosity =
         !OASISMessage.verbose
       in

         at_exit 
           (fun () ->
              FileUtil.rm [target_fn]);

         OASISMessage.verbose := false;
         file_generate 
           ~target:target_fn
           real_fn
           comment_fmt
           (NeedSplit content_lst);
         OASISMessage.verbose := verbosity;


         assert_equal 
           ~msg:"File content"
           ~printer:(Printf.sprintf "%S")
           (file_content expected_fn)
           (file_content target_fn))
  in

  "FileGenerate" >:::
  (
    List.map test_of_vector
      [
        "filegenerate1.txt", 
        [
          "toto";
          "# OASIS_START ";
          "# OASIS_STOP ";
        ], 
        comment_sh;

        "filegenerate2.txt", 
        [
          "toto";
          "# OASIS_START ";
          "# OASIS_STOP ";
        ], 
        comment_sh;

        "filegenerate3.txt", 
        [
          "toto";
          "# OASIS_START ";
          "# OASIS_STOP ";
        ], 
        comment_sh;

        "filegenerate4.txt", 
        [
          "toto";
          "# OASIS_START ";
          "# OASIS_STOP ";
        ], 
        comment_sh;

        "filegenerate5.txt", 
        [
          "toto";
          "# OASIS_START ";
          "tata";
          "# OASIS_STOP ";
        ], 
        comment_sh;
      ]
  )
;;
