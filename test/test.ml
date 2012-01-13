(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2008-2010, OCamlCore SARL                                    *)
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

(** Run tests for OASIS
    @author Sylvain Le Gall
  *)

open OUnit;;
open TestCommon;;

module SetString = Set.Make(String)

let _ =  
  let () = 
    OASISBuiltinPlugins.init ()
  in
  let () = 
    if Sys.os_type <> "Win32" then
      begin
        (* Check permission before anything else. This is a little bit
         * outside the scope of testing, but will make tests fail. We
         * also check for completeness of _fixperms files, which should
         * help OASIS devel to fix it early.
         *)
        let topdir = 
          Filename.parent_dir_name 
        in
        let exec_set =
          let rst = ref SetString.empty in
          let chn = open_in (Filename.concat topdir "_fixperms") in
          let () = 
            try 
              while true do 
                rst := 
                SetString.add 
                  (Filename.concat topdir (input_line chn))
                  !rst
              done;
            with End_of_file ->
              ()
          in
            close_in chn;
            !rst
        in
        let fn_build =
          FilePath.concat topdir "_build"
        in
        let (found_missing, unfound) =
          (* Check files that need to be executable *)
          FileUtil.find
            (FileUtil.And
               (FileUtil.Custom
                  (fun fn ->
                     not (FilePath.is_subdir fn fn_build)),
                FileUtil.And
                  (FileUtil.Is_exec,
                   FileUtil.And
                     (FileUtil.Is_file,
                      FileUtil.Not FileUtil.Is_link))))
            topdir
            (fun (found_missing, unfound) fn ->
               if SetString.mem fn unfound then
                 begin
                   (found_missing, SetString.remove fn unfound)
                 end
               else
                 begin
                   Printf.eprintf 
                     "E: File '%S' is executable but not in _fixperms\n%!"
                     fn;
                   (true, unfound)
                 end)
            (false, exec_set)
        in
          if unfound <> SetString.empty then
            begin
              SetString.iter 
                (fun fn ->
                   if FileUtil.test FileUtil.Exists fn then
                     Printf.eprintf
                       "E: File '%S' should be executable, run 'make fixperms'\n%!"
                       fn
                   else
                     Printf.eprintf
                       "E: File '%S' in _fixperms but doesn't exist, remove it\n%!"
                       fn)
                unfound;
              failwith "Fix your permissions"
            end
          else if found_missing then
            failwith "Fix your permissions"
      end
  in
    run_test_tt_main
      ~arg_specs:test_args 
      ~set_verbose
      ("OASIS">:::
       [
         TestPropList.tests;
         TestOASIS.tests;
         TestVersion.tests;
         TestFileTemplate.tests;
         TestBasic.tests;
         TestFull.tests;
         TestMETA.tests;
         TestLog.tests;
         TestLicense.tests;
         TestValues.tests;
         TestQuery.tests;
         TestQuickstart.tests;
         TestDevFiles.tests;
       ])
;;
