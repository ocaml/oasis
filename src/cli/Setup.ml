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
open BaseGenerate


let roasis_exec =
  ref None


let rupdate =
  ref NoUpdate


let main () =
  let oasis_setup_args =
    List.flatten
      [
        if !roasis_exec <> None then
          ["-real-oasis"]
        else
          [];
        if !CLICommon.ignore_plugins then
          ["-ignore-plugins"]
        else
          [];
        if !CLICommon.oasis_fn <> CLICommon.default_oasis_fn then
          ["-oasis"; !CLICommon.oasis_fn]
        else
          [];
        if !rupdate = Weak then
          ["-setup-update"; "weak"]
        else if !rupdate = Dynamic then
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
      ~oasis_fn:!CLICommon.oasis_fn
      ?oasis_exec:!roasis_exec
      ~oasis_setup_args
      !rupdate
      (OASISParse.from_file
         ~ctxt:!BaseContext.default
         !CLICommon.oasis_fn)
  in
    ()


let scmd =
  {(CLISubCommand.make
      "setup"
      (s_ "Translate _oasis into a build system")
      CLIData.setup_mkd
      main)
     with
         scmd_specs =
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
             s_ "mod Different ways to allow setup.ml to auto-update. \
                 The 'weak' is only triggered to regenerate setup.ml and all \
                 files when something change in `_oasis`. It has a weak \
                 dependency on the executable oasis, because it only needs it \
                 when `_oasis` is changed. The 'dynamic' mode has a strong \
                 dependency on the library oasis but it generates a very \
                 small `setup.ml`. If you want contributor to checkout your \
                 VCS and be able to work without oasis installed, prefer the \
                 'weak' mode. If you want to have very avoid VCS history \
                 pollution, use the 'dynamic' mode. Always distribute tarball
                 with mode 'none'."
           ] @ CLICommon.oasis_fn_specs}


let () =
  CLISubCommand.register_builtin scmd

