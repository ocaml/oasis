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

(** Tests for OASISFileTemplate
    @author Sylvain Le Gall
  *)

open OUnit
open TestCommon
open OASISFileTemplate

let tests =

  let test_of_vector (fn, content_lst, comment_fmt) = 
    fn >::
    bracket
      (fun () -> ref NoChange)
      (fun rchng ->
         let real_fn = 
           in_data fn
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

           rchng := file_generate 
                      ~ctxt:!oasis_ctxt
                      ~backup:true
                      (template_of_string_list
                         ~ctxt:!oasis_ctxt
                         ~template:true
                         real_fn 
                         comment_fmt
                         content_lst);

           assert_equal 
             ~msg:"File content"
             ~printer:(Printf.sprintf "%S")
             (file_content expected_fn)
             (file_content real_fn))

      (fun rchng -> file_rollback ~ctxt:!oasis_ctxt !rchng)

  in

  "FileTemplate" >:::
  (
    List.map test_of_vector
      [
        "filetemplate1.txt", 
        [
          "toto";
          "# OASIS_START ";
          "# OASIS_STOP ";
        ], 
        comment_sh;

        "filetemplate2.txt", 
        [
          "toto";
          "# OASIS_START ";
          "# OASIS_STOP ";
        ], 
        comment_sh;

        "filetemplate3.txt", 
        [
          "toto";
          "# OASIS_START ";
          "# OASIS_STOP ";
        ], 
        comment_sh;

        "filetemplate4.txt", 
        [
          "toto";
          "# OASIS_START ";
          "# OASIS_STOP ";
        ], 
        comment_sh;

        "filetemplate5.txt", 
        [
          "toto";
          "# OASIS_START ";
          "tata";
          "# OASIS_STOP ";
        ], 
        comment_sh;
      ]
  )
  @
  [
    "Keep file rights" >::
    (fun () ->
       let fn, chn = 
         Filename.open_temp_file "oasis-db" ".txt"
       in
         output_string 
           chn
           "# OASIS_START\n\
            # OASIS_STOP\n";
         close_out chn;
         try 
           let own, grp_org =
             let st = Unix.stat fn in
               st.Unix.st_uid, st.Unix.st_gid
           in

           let grp = 
             let lst =
               Array.to_list (Unix.getgroups ())
             in
               (* Try to find a group accessible to the user
                * and different from the current group 
                *)
               try 
                 List.find (fun gid' -> grp_org <> gid') lst
               with Not_found ->
                 skip_if true "No available group to change group of the file";
                 grp_org
           in

           let () = 
             Unix.chown fn own grp
           in

           let chng = 
             file_generate 
               ~ctxt:!oasis_ctxt
               ~backup:true
               (template_make
                  fn
                  comment_sh
                  []
                  ["echo Hello"]
                  [])
           in
             file_rollback ~ctxt:!oasis_ctxt chng;
             assert_equal
               ~msg:"File chgrp"
               ~printer:string_of_int
               grp
               ((Unix.stat fn).Unix.st_gid);
             Sys.remove fn

         with e ->
           Sys.remove fn;
           raise e)
  ]

;;
