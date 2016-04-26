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


(** Create the configure, build and install system
    @author Sylvain Le Gall
*)


open OASISGettext
open OASISUtils
open CLISubCommand
open OASISSetupUpdate


let fspecs () =
  let roasis_exec = ref None in
  let rupdate = ref NoUpdate in
  let specs =
    [
      "-real-oasis",
      Arg.Unit (fun () -> roasis_exec := Some (Sys.executable_name)),
      s_ " Use the real 'oasis' executable filename when generating \
          setup.ml.";
      "-setup-update",
      Arg.Symbol (["none"; "weak"; "dynamic"],
        (function
          | "none" -> rupdate := NoUpdate
          | "weak" -> rupdate := Weak
          | "dynamic" -> rupdate := Dynamic
          | str ->
            failwithf (f_ "Unknown setup-update mode %S") str)),
      s_ " Define the way `setup.ml` should update when `_oasis` change."
    ]
  in
  (specs, CLISubCommand.default_anon),
  (fun () -> !roasis_exec, !rupdate)


let main ~ctxt (oasis_exec, update) oasis_fn pkg =
  let oasis_setup_args =
    List.flatten
      [
        if oasis_exec <> None then
          ["-real-oasis"]
        else
          [];
        if ctxt.OASISContext.ignore_plugins then
          ["-ignore-plugins"]
        else
          [];
        if oasis_fn <> OASISParse.default_oasis_fn then
          ["-oasis"; oasis_fn]
        else
          [];
        if update = Weak then
          ["-setup-update"; "weak"]
        else if update = Dynamic then
          ["-setup-update"; "dynamic"]
        else
          [];
      ]
  in
  let _chngs: OASISFileTemplate.file_generate_change list =
    BaseGenerate.generate
      ~backup:false
      ~setup_fn:BaseSetup.default_filename
      ~restore:false
      ~oasis_fn
      ?oasis_exec
      ~oasis_setup_args
      update
      pkg
  in
  ()


let () =
  CLISubCommand.register "setup"
    (ns_ "Translate _oasis into a build system")
    CLIData.setup_mkd
    (CLICommon.parse_oasis_fn
       (CLISubCommand.make_run fspecs main))
