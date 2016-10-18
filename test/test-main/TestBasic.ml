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


let tests =
  "Basic" >:::
  [
    "Help1" >::
    (fun test_ctxt -> assert_oasis_cli ~ctxt:test_ctxt ["--help"]);

    "Help2" >::
    (fun test_ctxt -> assert_oasis_cli ~ctxt:test_ctxt ["help"]);

    "Manual" >::
    (fun test_ctxt -> assert_oasis_cli ~ctxt:test_ctxt ["manual"]);

    "Env dump/load" >::
    (fun test_ctxt ->
       let ctxt =
         {(TestCommon.oasis_ctxt test_ctxt) with
          OASISContext.srcfs =
            new OASISFileSystem.host_fs
              (in_testdata_dir test_ctxt ["TestBasic"])}
       in
       (* TODO: lock *)
       BaseEnv.unload ();
       BaseEnv.load
         ~ctxt
         ~filename:(OASISFileSystem.of_unix_filename "dir.data")
         ();
       (* Reset lazy values ? *)
       assert_equal
         ~printer:(fun s -> s)
         "toto"
         (BaseStandardVar.bindir ()));

    "OCaml dev version" >::
    (fun _ ->
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
    (fun _ ->
       assert_equal
         ~printer:(fun s -> s)
         OASISParse.default_oasis_fn
         BaseSetup.default_oasis_fn);

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
  ]
