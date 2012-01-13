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

open OUnit;;
open TestCommon;;

let tests =
  "Basic" >:::
  [
    "Help1" >::
    (fun () -> assert_oasis_cli ["--help"]);
 
    "Help2" >::
    (fun () -> assert_oasis_cli ["help"]);
 
    "Env dump/load" >::
    (fun () ->
       BaseEnv.unload ();
       BaseEnv.load ~filename:(in_data "dir.data") ();
       (* Reset lazy values ? *)
       assert_equal 
         ~printer:(fun s -> s)
         "toto"
         (BaseStandardVar.bindir ()));

    "OCaml dev version" >::
    (fun () ->
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
         (BaseStandardVar.ocaml_version ()))
  ]
;;
   
