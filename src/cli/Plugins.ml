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

(** Load extra action from plugins
    @author Sylvain Le Gall
  *)

open OASISGettext
open SubCommand
open PluginLoader

(** Plugin for the command line. *)
let plugin_cli_t () =
  {
    PluginLoader.
    system = "oasis-cli";
    msg    = !BaseContext.default.OASISContext.printf
  }

(** Plugin for handling _oasis. *)
let plugin_pkg_t () =
  {(plugin_cli_t ()) with system = "oasis"}

(** Initialization and general command-line argument. *)
let () =
  PluginLoader.init
    PluginsLoaded.exec_oasis_build_depends_rec;
  ArgExt.add_global_options
    [
      "--plugin-cli",
      Arg.String
        (fun str ->
           PluginLoader.load (plugin_cli_t ()) str),
      (s_ "nm Load a plugin.")
    ]

(** Display long help. *)
let long = ref false

let main () =
  let print =
    function
      | [] ->
          print_endline (s_ "<none>")
      | lst ->
          List.iter
            (fun e ->
               let synopsis =
                 match e.synopsis with
                   | Some str -> str
                   | None -> s_ "<none>"
               in
                 begin
                   match e.version with
                     | Some ver ->
                         Printf.printf
                           (f_ "%s (v%s): %s\n")
                           e.name ver synopsis
                     | None ->
                         Printf.printf
                           (f_ "%s: %s\n")
                           e.name synopsis
                 end;
                 if !long then
                   begin
                     Printf.printf (f_ "Findlib name: %s\n") e.findlib_name;
                     match e.description with
                       | Some str ->
                           Printf.printf (f_ "Description:\n%s\n") str
                       | None ->
                           ()
                   end;
                 Printf.printf "%!")
            lst
  in
    print_endline
      (s_ "Command-line plugins, use --plugin-cli command \
           line option to load:");
    print (PluginLoader.list (plugin_cli_t ()));
    print_newline ();
    print_endline
      (s_ "_oasis plugins, loaded if present in _oasis:");
    print (PluginLoader.list (plugin_pkg_t ()))

let scmd =
  {(SubCommand.make
      ~std_usage:true
      "plugin-list"
      (s_ "List available plugin")
      CLIData.plugin_mkd
      main) with
          scmd_specs =
            [
              "--long",
              Arg.Set long,
              " Display a long description for plugin."
            ]}

let () =
  SubCommand.register scmd

