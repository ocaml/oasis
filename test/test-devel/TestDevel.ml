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


let () =
  run_test_tt_main
    ~exit
    ("OASIS">:::
     [
       (* Keep sorted. *)
       TestSelfCompile.tests;

       (* Extra small tests. *)
       "setup.ml has been generated with a previous version of oasis" >::
       (fun test_ctxt ->
          let oasis_version_for_setup_ml =
            let buf = Buffer.create 13 in
            OUnit2.assert_command
              ~chdir:".."
              ~ctxt:test_ctxt
              ~foutput:(Stream.iter (Buffer.add_char buf))
              "ocaml" ["setup.ml"; "-version"];
            OASISString.trim (Buffer.contents buf)
          in
          let oasis_version_current =
            OASISVersion.string_of_version OASISConf.version_full
          in
          assert_bool
            (Printf.sprintf
               "The OASIS version for the generated setup.ml should be '< %s', \
                but this is currently %S."
               oasis_version_current
               oasis_version_for_setup_ml)
            (0 <
             (OASISVersion.StringVersion.compare
                oasis_version_current
                oasis_version_for_setup_ml)));
     ]);
