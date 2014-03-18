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


(** Tests the subcommand query of OASIS
    @author Sylvain Le Gall
  *)


open TestCommon
open OUnit2


let tests =
  let test_of_vector (fn, qa) =
    let q, a =
      List.split qa
    in
    let test_name =
      Printf.sprintf
        "query('%s', %s)"
        fn (String.concat ", " q)
    in
      test_name >::
      (fun test_ctxt ->
         assert_oasis_cli
           ~ctxt:test_ctxt
           ~output:((String.concat "\n" a)^"\n")
           ~unorder:true
           (["-quiet"; "query"; "-oasis";
             in_testdata_dir test_ctxt ["TestQuery"; fn]] @ q))
  in

    "query" >:::
    (List.map test_of_vector
       [
         "test1.oasis",
         ["version", "0.0.1";
          "name", "oasis"];

         "test1.oasis",
         ["Flag(devmod).Default", "false"];

         "test10.oasis",
         ["ListSections",
          "Test(main)\nFlag(test)"];

         "test10.oasis",
         ["ListFields",
          "OASISFormat\
           \nName\
           \nVersion\
           \nSynopsis\
           \nLicenseFile\
           \nAuthors\
           \nCopyrights\
           \nMaintainers\
           \nLicense\
           \nConfType\
           \nBuildType\
           \nInstallType\
           \nCategories\
           \nFilesAB\
           \nPlugins\
           \nBuildDepends\
           \nBuildTools\
           \nXDevFilesMakefileNoTargets\
           \nXDevFilesEnableMakefile\
           \nXDevFilesEnableConfigure\
           \nXStdFilesREADME\
           \nXStdFilesREADMEFilename\
           \nXStdFilesINSTALL\
           \nXStdFilesINSTALLFilename\
           \nXStdFilesAUTHORS\
           \nXStdFilesAUTHORSFilename\
           \nTest(main).Run\
           \nTest(main).Type\
           \nTest(main).TestTools\
           \nFlag(test).Description"]
       ])
