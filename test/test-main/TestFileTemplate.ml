(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2016, Sylvain Le Gall                                   *)
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


(** Tests for OASISFileTemplate
    @author Sylvain Le Gall
  *)


open OUnit2
open TestCommon
open OASISFileTemplate


let printer_change =
  function
    | Create fn -> Printf.sprintf "Create %S" fn
    | Change (fn, Some fn') ->
        Printf.sprintf "Change (%S, Some %s)" fn fn'
    | Change (fn, None) ->
        Printf.sprintf "Change (%S, None)" fn
    | NoChange ->
        "NoChange"

let tests =

  let test_of_vector (fn, content_lst, comment_fmt) =
    fn >::
    (fun test_ctxt ->
       let real_fn = in_testdata_dir test_ctxt ["TestFileTemplate"; fn] in
       let tmpdir = bracket_tmpdir test_ctxt in
       let expected_fn = real_fn ^ "-exp" in

       let tmp_fn =
         (* Copy file to temporary. *)
         if Sys.file_exists real_fn then
           FileUtil.cp [real_fn] tmpdir;
         Filename.concat tmpdir (Filename.basename real_fn)
       in

       let chng: file_generate_change =
         file_generate
           ~ctxt:(oasis_ctxt test_ctxt)
           ~backup:true
           (template_of_string_list
              ~ctxt:(oasis_ctxt test_ctxt)
              ~template:true
              tmp_fn
              comment_fmt
              content_lst)
       in
         assert_equal
           ~msg:"File content"
           ~printer:(Printf.sprintf "%S")
           (file_content expected_fn)
           (file_content tmp_fn);

         file_rollback ~ctxt:(oasis_ctxt test_ctxt) chng;

         if Sys.file_exists real_fn then begin
           assert_equal
             ~msg:"File content back to pristine."
             (file_content real_fn)
             (file_content tmp_fn);

           FileUtil.rm [tmp_fn];
         end;

         assert_equal
           ~msg:"Temporary directory empty."
           0
           (List.length (FileUtil.ls tmpdir)))
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
    (fun test_ctxt ->
       let () =
         skip_if (Sys.os_type = "Win32") "UNIX only test"
       in
       let dn = bracket_tmpdir test_ctxt in
       let fn = Filename.concat dn "foo.sh" in
       let chn = open_out fn in
       let () =
         output_string
           chn
           "# OASIS_START\n\
            # OASIS_STOP\n";
         close_out chn
       in
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
           ~ctxt:(oasis_ctxt test_ctxt)
           ~backup:true
           (template_make
              fn
              comment_sh
              []
              ["echo Hello"]
              [])
       in
         file_rollback ~ctxt:(oasis_ctxt test_ctxt) chng;
         assert_equal
           ~msg:"File chgrp"
           ~printer:string_of_int
           grp
           ((Unix.stat fn).Unix.st_gid));

    "bug1382-keep all eol" >::
    (fun test_ctxt ->
       let dn = bracket_tmpdir test_ctxt in
       let fn = Filename.concat dn "foo.txt" in
       let ghost_meta_template =
         template_make fn comment_meta [] ["nothing"; ""; "bar"] []
       in
         assert_equal
           ~printer:printer_change
           (Create fn)
           (file_generate ~ctxt:(oasis_ctxt test_ctxt) ~backup:false ghost_meta_template);
         assert_equal
           ~printer:printer_change
           NoChange
           (file_generate ~ctxt:(oasis_ctxt test_ctxt) ~backup:false ghost_meta_template);
         ());
  ]
