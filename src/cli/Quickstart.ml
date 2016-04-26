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


(** Run a oasis writer helper
    @author Sylvain Le Gall
*)


open OASISGettext
open OASISQuickstart
open OASISTypes
open CLISubCommand


let main ~ctxt (qckstrt_lvl, interf, setup_data) oasis_fn =
  let post_run () =
    let pkg = OASISParse.from_file ~ctxt oasis_fn in
    Setup.main ~ctxt setup_data oasis_fn pkg
  in
  OASISQuickstart.to_file ~ctxt oasis_fn qckstrt_lvl interf post_run


let fspecs () =
  let (specs, anon), gen = Setup.fspecs () in
  let lvls =
    [
      "beginner", Beginner;
      "intermediate", Intermediate;
      "expert", Expert;
    ]
  in
  let qckstrt_lvl = ref Beginner in
  let interf = ref Human in
  let specs' =
    ["-level",
     Arg.Symbol
       ((List.map fst lvls),
        (fun s -> qckstrt_lvl := List.assoc s lvls)),
     (s_ " Quickstart level, skip questions according to this \
          level.");
     "-machine",
     Arg.Unit (fun () -> interf := Machine),
     (s_ " Computer readable questions for automatic completion.")] @ specs
  in
  let gen' () = !qckstrt_lvl, !interf, gen () in
  (specs', anon), gen'


let () =
  CLISubCommand.register "quickstart"
    (ns_ "Launch an helper to write `_oasis` file")
    CLIData.quickstart_mkd
    (CLICommon.define_oasis_fn
       (CLISubCommand.make_run fspecs main))
