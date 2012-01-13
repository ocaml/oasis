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

(** Test the devfiles plugin
   
    @author Sylvain Le Gall
  *)

open TestCommon
open OUnit

(* TODO: move this to OUnit *)
let bracket_tmpdir f = 
  bracket
    (fun () ->
       let dn =
         Filename.temp_file "oasis-db-" ".dir"
       in
         FileUtil.rm [dn];
         FileUtil.mkdir dn;
         dn)
    f
    (fun dn ->
       FileUtil.rm ~recurse:true [dn])

let tests = 
  "DevFiles" >::
  (bracket_tmpdir 
     (fun dn ->
        bracket 
          (fun () ->
             let pwd = FileUtil.pwd () in
               Sys.chdir dn;
               pwd)
          (fun _ ->
             FileUtil.cp 
               [in_data "test-devfiles1.oasis"] 
               "_oasis";
             assert_command 
               (oasis ()) (!oasis_args @ ["setup"]);
             assert_command
               "./configure" ["--prefix=/usr"; "--mandir=/usr/share/man"; 
                              "--infodir=/usr/share/info"; "--datadir=/usr/share";
                              "--sysconfdir=/etc"; "--localstatedir=/var/lib"];)
          (fun pwd ->
             Sys.chdir pwd)
          ()))



