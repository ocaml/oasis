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

(* Compile OASIS itself using the version under tests. *)

open OUnit2
open TestCommon

let tests =
  "SelfCompile" >:
  (test_case ~length:OUnitTest.Long
    (fun test_ctxt ->
       let () = skip_long_test test_ctxt in
       let pwd = FileUtil.pwd () in
       let src_dir = Filename.dirname pwd in
       let in_src_dir fn = Filename.concat src_dir fn in
       let () =
         skip_if
           (Sys.os_type <> "Unix" || not (Sys.file_exists (in_src_dir ".git")))
           "Only compile for particular dev configurations."
       in
       let tmpdir = bracket_tmpdir test_ctxt in
       let rlst =
         let ignore_dot_git fn' =
           OASISString.starts_with ~what:(in_src_dir ".git") fn'
         in
         ref [ignore_dot_git]
       in
       let () =
         (* Take into .gitignore to prevent copying some files. *)
         let chn = open_in (in_src_dir ".gitignore") in
         try
           while true do
             let pat = input_line chn in
             let test =
               if pat <> "" then begin
                 let pat_len = String.length pat in
                 let no_first_char = String.sub pat 1 (pat_len - 1) in
                 match String.get pat 0, String.get pat (pat_len - 1) with
                 | '/', '/' ->
                   if pat_len = 1 then
                     fun _ -> true
                   else
                     let fn = in_src_dir (String.sub pat 1 (pat_len - 2)) in
                     fun s -> OASISString.starts_with ~what:fn s
                 | '/', _ ->
                   let fn = in_src_dir no_first_char in
                   fun s -> fn = s
                 | '*', _ ->
                   fun s -> OASISString.ends_with ~what:no_first_char s
                 | _ ->
                   fun s -> OASISString.ends_with ~what:pat s
               end else begin
                 fun _ -> false
               end
             in
             rlst := test :: !rlst
           done;
         with End_of_file ->
           close_in chn
       in
       let is_ignored fn = List.exists (fun test -> test fn) !rlst in
       (* List all files that should be copied. *)
       let files =
         FileUtil.find FileUtil.Is_file src_dir (fun lst fn -> fn :: lst) []
       in
       (* Copy all files to a temporary directory. *)
       let () =
         List.iter
           (fun fn ->
              if not (is_ignored fn) then begin
                let target_fn = FilePath.reparent src_dir tmpdir fn in
                let dn = Filename.dirname target_fn in
                logf test_ctxt `Info "Copy %s -> %s." fn target_fn;
                if not (Sys.file_exists dn) then
                  FileUtil.mkdir ~parent:true dn;
                FileUtil.cp [fn] target_fn
              end)
           files
       in
         (* Regenerate setup.ml and try to compile. *)
         assert_oasis_cli ~ctxt:test_ctxt ~chdir:tmpdir ["setup"];
         assert_command ~ctxt:test_ctxt ~chdir:tmpdir
           "ocaml" ["setup.ml"; "-configure";
                    "--override"; "is_native";
                    string_of_bool (is_native test_ctxt);
                    "--override"; "native_dynlink";
                    string_of_bool (native_dynlink test_ctxt)];
         assert_command ~ctxt:test_ctxt ~chdir:tmpdir
           "ocaml" ["setup.ml"; "-build"]))
