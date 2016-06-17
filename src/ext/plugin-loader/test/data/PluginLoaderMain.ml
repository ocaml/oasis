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

type action = List | Load of string list


let verbose = ref false


let () =
  let action = ref List in
  let () =
    Arg.parse
      ["-load",
       Arg.String(fun str ->
         action :=
           match !action with
             | List -> Load [str]
             | Load lst -> Load (lst @ [str])),
       "nm Load a plugin."]
      ignore
      (Sys.executable_name^" [options*]")
  in
  let () =
    PluginLoader.init ["pluginloaderLib"]
  in
  let t =
    {PluginLoader.
      system = "pluginloader";
      msg =
        fun lvl str ->
          let prefix =
            match lvl with
              | `Error -> "E"
              | `Warning -> "W"
              | `Debug -> "D"
          in
          if !verbose then
            prerr_endline (prefix^": "^str)}
  in
  try
    match !action with
      | List ->
        List.iter
          (fun entry ->
             print_endline
               (entry.PluginLoader.name^": "^
                  (match entry.PluginLoader.synopsis with
                    | Some str -> str
                    | None -> "<none>")))
          (PluginLoader.list t)
      | Load lst ->
        let _t =
          List.iter (PluginLoader.load t) lst
        in
        print_endline
          ("plugin_loaded: "^
             (String.concat ", "!PluginloaderLib.registered_plugins))
  with e ->
    prerr_endline (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    exit 1


