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


(** Main for OASIS *)


open MainGettext


open OASISTypes
open OASISUtils
open OASISPlugin
open OASISBuiltinPlugins
open BaseMessage
open CLISubCommand
open Format
open FormatExt


open Setup
open SetupDev
open SetupClean
open Quickstart
open Manual
open Check
open Query
open Version
open Help


let () =
  (* Run subcommand *)
  try
    OASISBuiltinPlugins.init ();
    CLIArgExt.parse_and_run ()
  with e ->
    begin
      if Printexc.backtrace_status () then
        Printexc.print_backtrace stderr;

      begin
        match e with
          | Failure str ->
            error "%s" str
          | e ->
            begin
              error "%s" (Printexc.to_string e);
              if Printexc.backtrace_status () then
                List.iter
                  (debug "%s")
                  (OASISString.nsplit (Printexc.get_backtrace ()) '\n')
            end
      end;

      exit 1
    end
