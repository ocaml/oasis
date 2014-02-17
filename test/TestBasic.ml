(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2013, Sylvain Le Gall                                   *)
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


let tests =
  "Basic" >:::
  [
    "Help1" >::
    (fun test_ctxt -> assert_oasis_cli ~ctxt:test_ctxt ["--help"]);

    "Help2" >::
    (fun test_ctxt -> assert_oasis_cli ~ctxt:test_ctxt ["help"]);

    "Env dump/load" >::
    (fun test_ctxt ->
       (* TODO: lock *)
       BaseEnv.unload ();
       BaseEnv.load ~filename:(in_testdata_dir test_ctxt
                                 ["TestBasic"; "dir.data"]) ();
       (* Reset lazy values ? *)
       assert_equal
         ~printer:(fun s -> s)
         "toto"
         (BaseStandardVar.bindir ()));

    "OCaml dev version" >::
    (fun test_ctxt ->
       PropList.Schema.set
         BaseEnv.schema
         BaseEnv.env
         ~context:BaseEnv.OCommandLine
         "ocamlc_config_map"
         (Marshal.to_string
            (MapString.add
               "version"
               "3.13.0+dev2 (2010-10-22)"
               MapString.empty)
            []);
       assert_equal
         ~printer:(fun s -> s)
         "3.13.0"
         (BaseStandardVar.ocaml_version ()));

    "Compileable BaseSysBundle.ml" >::
    (fun test_ctxt ->
       let dn = bracket_tmpdir test_ctxt in
       let chn = open_out (Filename.concat dn "bundle.ml") in
         output_string chn OASISData.oasissysbundle_ml;
         output_string chn BaseData.basesysbundle_ml;
         close_out chn;
         assert_command ~ctxt:test_ctxt
           "ocamlc" ["-o"; (Filename.concat dn "bundle");
                     (Filename.concat dn "bundle.ml")]);

    "Sync OASISParse/BaseSetup.default_oasis_fn" >::
    (fun test_ctxt ->
       assert_equal
         ~printer:(fun s -> s)
         OASISParse.default_oasis_fn
         BaseSetup.default_oasis_fn);
  ]
