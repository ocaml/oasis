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


(** Help on subcommands
*)


open CLISubCommand
open CLIArgExt
open OASISGettext


let main ~ctxt scmd_name =
  let hext =
    match scmd_name with
      | None -> NoSubCommand
      | Some "all" -> AllSubCommand
      | Some nm -> (SubCommand nm)
  in
  let pager, fmt =
    CLIPager.open_out ()
  in
  try
    pp_print_help ~ctxt hext Output fmt ();
    CLIPager.close_out pager
  with e ->
    CLIPager.close_out pager;
    raise e


let () =
  CLISubCommand.register "help"
    (ns_ "Display help for a subcommand")
    CLIData.help_mkd
    ~usage:(ns_ "[subcommand|all]")
    (CLISubCommand.make_run
       (fun () ->
          let scmd_name = ref None in
          ([], (fun s -> scmd_name := Some s)),
          (fun () -> !scmd_name))
       main)

