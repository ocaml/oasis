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

(** Display the manual
    @author Sylvain Le Gall
  *)

open SubCommand
open OASISGettext

let output = 
  ref None

let main () = 
  let fmt, chn = 
    match !output with 
      | Some fn ->
          let chn = 
            open_out fn
          in
            Format.formatter_of_out_channel chn,
            chn

      | None ->
          Format.std_formatter,
          stdout
  in
    OASISHelp.pp_print_help 
      fmt

      (* CLI help *)
      (ArgExt.pp_print_help ArgExt.AllSubCommand ArgExt.Markdown)

      (* Fields from schema *)
      BaseEnv.schema

      (* Environment variable *)
      (let lst = 
         BaseEnv.var_all ()
       in
         fun nm _ ->
           List.mem nm lst);
    (* TODO: don't close if we are called from another function *)
    close_out chn

let scmd = 
  {(SubCommand.make
      "manual"
      (s_ "Display user manual")
      CLIData.manual_mkd
      main)
     with 
         scmd_specs = ["-o", 
                       Arg.String (fun s -> output := Some s),
                       "fn Output manual to filename."]}

let () = 
  SubCommand.register scmd
