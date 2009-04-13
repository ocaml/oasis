
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
         !BaseMessage.verbose
       in

         at_exit 
           (fun () ->
              if Sys.file_exists target_fn then
                Sys.remove target_fn);

         BaseMessage.verbose := false;
         file_generate 
           ~target:target_fn
           real_fn
           content_lst
           comment_fmt;
         BaseMessage.verbose := verbosity;


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
          "# AUTOBUILD_START ";
          "# AUTOBUILD_STOP ";
        ], 
        comment_sh;

        "filegenerate2.txt", 
        [
          "toto";
          "# AUTOBUILD_START ";
          "# AUTOBUILD_STOP ";
        ], 
        comment_sh;

        "filegenerate3.txt", 
        [
          "toto";
          "# AUTOBUILD_START ";
          "# AUTOBUILD_STOP ";
        ], 
        comment_sh;

        "filegenerate4.txt", 
        [
          "toto";
          "# AUTOBUILD_START ";
          "# AUTOBUILD_STOP ";
        ], 
        comment_sh;

        "filegenerate5.txt", 
        [
          "toto";
          "# AUTOBUILD_START ";
          "tata";
          "# AUTOBUILD_STOP ";
        ], 
        comment_sh;
      ]
  )
;;
