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

(** Help on subcommands 
  *)

open SubCommand 
open ArgExt
open MainGettext

let scmd_name = 
  ref None

let main () = 
  let pp_print_help =
    match !scmd_name with 
      | None ->
          pp_print_help NoSubCommand
      | Some "all" ->
          pp_print_help AllSubCommand
      | Some nm ->
          pp_print_help (SubCommand nm)
  in
    pp_print_help Output Format.std_formatter ()

let scmd = 
  {(SubCommand.make
      "help"
      (s_ "Display help for a subcommand")
      CLIData.help_mkd
      main)
     with 
         scmd_usage = s_ "[subcommand|all]";
         scmd_anon  = (fun s -> scmd_name := Some s)}

let () = 
  SubCommand.register scmd
    
