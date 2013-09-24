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
open OUnit2

let tests = 
  "DevFiles" >::
  (fun test_ctxt ->
     let dn = bracket_tmpdir test_ctxt in
     FileUtil.cp 
       [in_testdata_dir test_ctxt ["test-devfiles1.oasis"]]
       (Filename.concat dn "_oasis");
     assert_oasis_cli ~ctxt:test_ctxt ~chdir:dn ["setup"];
     if Sys.os_type <> "Win32" then
       assert_command ~ctxt:test_ctxt ~chdir:dn
         "./configure" ["--prefix=/usr"; "--mandir=/usr/share/man"; 
                        "--infodir=/usr/share/info"; "--datadir=/usr/share";
                        "--sysconfdir=/etc"; "--localstatedir=/var/lib"];
     ())
