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
open TestCommon
open OASISTypes

let tests =
  "OASISObject" >::
  (fun test_ctxt ->
     let in_dir lst =
       in_testdata_dir test_ctxt
         (["TestOASISObject"; "source_patterns"] @ lst)
     in
     let ctxt = oasis_ctxt test_ctxt in
     let sfs = new OASISFileSystem.host_fs (in_dir []) in
     let pkg = OASISParse.from_file ~ctxt (in_dir ["_oasis"]) in
     let lst =
       List.fold_left
         (fun acc sct ->
            match sct with
            | Object (cs, bs, obj) ->
              let lst =
                OASISObject.source_unix_files
                  ~ctxt
                  (cs, bs, obj)
                  (fun fn ->
                     sfs#file_exists (OASISFileSystem.of_unix_filename fn))
              in
              (List.flatten (List.rev_map snd lst)) @ acc
            | Library _ | Executable _ | Flag _ | SrcRepo _ | Test _ | Doc _ ->
              acc)
         [] pkg.sections
     in
     DiffSetOutput.assert_equal
       (DiffSetOutput.of_list ["O1.eliom"; "O1.eliomi"; "O2.ml"])
       (DiffSetOutput.of_list lst))
