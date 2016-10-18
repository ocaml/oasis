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
