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


(** Load extra action from plugins
    @author Sylvain Le Gall
  *)


open OASISGettext
open CLISubCommand
open PluginLoader


(** Plugin for handling _oasis. *)
let plugin_pkg_t () =
  {
    PluginLoader.
    system = "oasis";
    msg    = !BaseContext.default.OASISContext.printf
  }


let main ~ctxt long =
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
                 if long then
                   Printf.printf (f_ "Findlib name: %s\n") e.findlib_name;
                 Printf.printf "%!")
            lst
  in
    print_endline
      (s_ "Command-line plugins, use --plugin-cli command \
           line option to load:");
    print (CLISubCommand.list_plugin ());
    print_newline ();
    print_endline
      (s_ "_oasis plugins, loaded if present in _oasis:");
    print (PluginLoader.list (plugin_pkg_t ()))


let () =
  CLISubCommand.register "plugin-list"
    (ns_ "List available plugin")
    CLIData.plugin_mkd
    (CLISubCommand.make_run
       (fun () ->
          let long = ref false in
            ([
              "--long",
              Arg.Set long,
              " Display a long description for plugin."],
             CLISubCommand.default_anon),
            (fun () -> !long))
       main)
