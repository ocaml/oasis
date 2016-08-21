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


(** Run tests for OASIS
    @author Sylvain Le Gall
  *)


open OUnit2


let extract_timings () =
  let log_fn j =
    Filename.concat OUnitUtils.buildir
      (Printf.sprintf "oUnit-OASIS-%s.log" (OUnitUtils.shardf j))
  in
  let j = ref 0 in
  let timings = Hashtbl.create 13 in
  let () = ignore "(*(*" in
  let rex = Pcre.regexp "Time spent in '(.*)': (.*)s" in
  let total_time = ref 0.0 in
  let () =
    while Sys.file_exists (log_fn !j) do
      let chn = open_in (log_fn !j) in
        incr j;
        try
          while true do
            let ln = input_line chn in
              try
                let substr = Pcre.exec ~rex ln in
                let name = Pcre.get_substring substr 1 in
                let time = float_of_string (Pcre.get_substring substr 2) in
                let count', time' =
                  try
                    Hashtbl.find timings name
                  with Not_found ->
                    0, 0.0
                in
                  total_time := !total_time +. time;
                  Hashtbl.replace timings name (count' + 1, time' +. time)
              with Not_found ->
                ()
          done
        with End_of_file ->
          close_in chn
    done
  in
  let output_dir = "../dist" in
  let output_fn = Filename.concat output_dir "timings.csv" in
  if Sys.file_exists output_dir && Sys.is_directory output_dir then begin
    let chn = open_out output_fn in
      Printf.fprintf chn "name,time,count\n";
      Hashtbl.iter
        (fun name (count, time) ->
           Printf.fprintf chn "%S,%f,%d\n" name time count)
        timings;
      close_out chn
  end;
  try
    ignore(Sys.getenv "TIMINGS");
    Hashtbl.iter
      (fun name (count, time) ->
         Printf.printf
           "Time spent in '%s':\n  % 7.2fs (% 3d time, % 6.2f%%, %5.2fs/call)\n"
           name time count ((time /. !total_time) *. 100.0)
           (time /. (float_of_int count)))
      timings;
    Printf.printf "Total time accounted: %fs\n" !total_time
  with Not_found ->
    ()

let () =
  let () =
    OASISBuiltinPlugins.init ()
  in
    run_test_tt_main
      ~exit
      ("OASIS">:::
       [
         (* Keep sorted. *)
         TestBaseCompat.tests;
         TestBaseEnv.tests;
         TestBaseLog.tests;
         TestBasic.tests;
         TestExamples.tests;
         TestFileTemplate.tests;
         TestFull.tests;
         TestLicense.tests;
         TestOASISAst.tests;
         TestOASISFileSystem.tests;
         TestOASISLibrary.tests;
         TestOASISObject.tests;
         TestOASISParse.tests;
         TestOASISSourcePatterns.tests;
         TestOASISString.tests;
         TestOASISText.tests;
         TestPluginDevFiles.tests;
         TestPluginInternal.tests;
         TestPluginMETA.tests;
         TestPluginOCamlbuild.tests;
         TestPluginOMake.tests;
         TestPluginStdFiles.tests;
         TestPropList.tests;
         TestQuery.tests;
         TestQuickstart.tests;
         TestSelfCompile.tests;
         TestValues.tests;
         TestVersion.tests;
       ]);
      extract_timings ()
