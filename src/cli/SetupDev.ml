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


(** Like Setup but in development mode (deprecated).
    @author Sylvain Le Gall
  *)


open SubCommand
open OASISGettext


let main () =
  failwith (s_ "The SetupDev subcommand is deprecated, use Setup")


let scmd =
  {(SubCommand.make
      ~std_usage:true
      "setup-dev"
      (s_ "Translate _oasis into a build system that auto-update (deprecated).")
      CLIData.setup_dev_mkd
      main)
     with
         scmd_specs =
           ([
             "-real-oasis",
             Arg.Unit ignore,
             s_ " Use the real 'oasis' executable filename when generating \
                  developper mode setup.ml.";

             "-run",
             Arg.Rest ignore,
             s_ " Run a command after generating files, this is the mode used \
                  by setup.ml in developper mode. Don't use it directly.";

             "-only-setup",
             Arg.Set (ref false),
             s_ " When generating the build system, keep only setup.ml and \
                  delete other generated files.";
           ] @ ArgCommon.oasis_fn_specs)}


let () =
  SubCommand.register scmd
