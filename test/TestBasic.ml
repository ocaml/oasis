(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2008-2010, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

open OUnit;;
open TestCommon;;

let tests ctxt =
  "Basic" >:::
  [
    "Help" >::
    (fun () -> 
       assert_command
         ~exit_code:0
         ctxt
         ctxt.oasis
         (ctxt.oasis_args @ ["--help"]));
 
    "Env dump/load" >::
    (fun () ->
       BaseEnv.unload ();
       BaseEnv.load ~filename:(in_data "dir.data") ();
       (* Reset lazy values ? *)
       assert_equal 
         ~printer:(fun s -> s)
         "toto"
         (BaseStandardVar.bindir ()));
  ]
;;
   
