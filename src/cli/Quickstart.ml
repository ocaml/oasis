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

(** Run a oasis writer helper
    @author Sylvain Le Gall
  *)

open OASISGettext
open OASISQuickstart
open OASISTypes
open SubCommand

let qckstrt_lvl =
  ref Beginner

let interf = 
  ref Human

let main () =
  OASISQuickstart.to_file 
    ~ctxt:!BaseContext.default
    !ArgCommon.oasis_fn
    !qckstrt_lvl
    !interf
    Setup.main

let scmd =
  let lvls =
    [
      s_ "beginner", Beginner; 
      s_ "intermediate", Intermediate; 
      s_ "expert", Expert;
    ]
  in
    {(SubCommand.make
        ~std_usage:true
        "quickstart"
        (s_ "Launch an helper to write `_oasis` file")
        CLIData.quickstart_mkd
        main)
       with 
           scmd_specs =
             (
               "-level",
               Arg.Symbol
                 ((List.map fst lvls),
                  (fun s -> qckstrt_lvl := List.assoc s lvls)),
               (s_ " Quickstart level, skip questions according to this level.")
             ) 
             ::
             (
               "-machine",
               Arg.Unit (fun () -> interf := Machine), 
               (s_ " Computer readable questions for automatic completion.")
             )
             :: SetupDev.scmd.scmd_specs}

let () = 
  SubCommand.register scmd
