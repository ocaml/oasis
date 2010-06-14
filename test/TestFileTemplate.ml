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

open OUnit;;
open TestCommon;;
open OASISFileTemplate;;

let tests ctxt =

  let test_of_vector (fn, content_lst, comment_fmt) = 
    fn >::
    bracket
      (fun () ->
         !OASISMessage.verbose,
         Filename.temp_file "filetemplate" ".txt")
      (fun (verbosity, target_fn) ->
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

           OASISMessage.verbose := false;
           file_generate 
             (let t = 
                of_string_list
                  ~template:true
                  real_fn 
                  comment_fmt
                  content_lst
              in
                {t with tgt_fn = Some target_fn});

           assert_equal 
             ~msg:"File content"
             ~printer:(Printf.sprintf "%S")
             (file_content expected_fn)
             (file_content target_fn))

      (fun (verbosity, target_fn) ->
         OASISMessage.verbose := verbosity;
         FileUtil.rm [target_fn])

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
;;
