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


open OASISTypes
open OASISUtils
open OASISGettext

(** Plugin for the command line. *)
let plugin_cli_t () =
  {
    PluginLoader.
    system = "oasis-cli";
    msg =
      (fun lvl str ->
         OASISMessage.generic_message ~ctxt:!BaseContext.default lvl "%s" str)
  }


type t =
    {
      scmd_name:      string;
      scmd_synopsis:  string;
      scmd_help:      string;
      scmd_specs:     (Arg.key * Arg.spec * Arg.doc) list;
      scmd_usage:     string;
      scmd_anon:      string -> unit;
      scmd_main:      unit -> unit;
    }

type registered_t = Builtin of t | Plugin of (PluginLoader.entry * t option)


let make ?(std_usage=false) nm snps hlp main =
  {
    scmd_name      = nm;
    scmd_synopsis  = snps;
    scmd_help      = hlp;
    scmd_specs     = [];
    scmd_usage     = if std_usage then s_ "[options*]" else "";
    scmd_anon      = (failwithf (f_ "Don't know what to do with '%s'."));
    scmd_main      = main;
  }


(* TODO: protect with mutex. *)
let all_frozen = ref false
let all = Hashtbl.create 20


let init () =
  (* TODO: only_once *)
  PluginLoader.init CLIPluginsLoaded.exec_oasis_build_depends_rec;
  List.iter
    (fun e ->
       Hashtbl.add all e.PluginLoader.name (Plugin(e, None)))
    (PluginLoader.list (plugin_cli_t ()))


let freeze () =
  all_frozen := true


let register_builtin t =
  (* Check that we are replacing. *)
  if !all_frozen then
    failwithf
      (f_ "Trying to register builtin subcommand %s after initialization.")
      t.scmd_name;
  Hashtbl.add all t.scmd_name (Builtin t)


let register_plugin t =
  let merge_option opt txt =
    match opt with
      | Some txt -> txt
      | None -> txt
  in

  try
    match Hashtbl.find all t.scmd_name with
      | Builtin _ ->
          failwithf
            (f_ "Trying to replace the builtin subcommand %s with \
                 an external plugin.")
            t.scmd_name
      | Plugin (e, _) ->
          let t' =
            {t with
              scmd_synopsis = merge_option
                                e.PluginLoader.synopsis
                                t.scmd_synopsis}
          in
            Hashtbl.replace all t.scmd_name (Plugin(e, Some t'))
  with Not_found ->
    let fake_entry =
      {
        PluginLoader.
        findlib_name = "<none>";
        name = t.scmd_name;
        synopsis = Some t.scmd_synopsis;
        version = None
      }
    in
      Hashtbl.add all t.scmd_name (Plugin(fake_entry, Some t))


let find nm =
  let rec find' retry =
    try
      match Hashtbl.find all nm with
        | Builtin t
        | Plugin(_, Some t) -> t
        | Plugin(e, None) ->
            if retry then
              failwithf
                (f_ "Loading findlib %s should register subcommand %s, but the \
                     loading didn't registered it.")
                e.PluginLoader.findlib_name nm;
            PluginLoader.load (plugin_cli_t ()) nm;
            find' true
    with Not_found ->
      failwithf (f_ "Subcommand '%s' doesn't exist") nm
  in
    find' false


let list_plugin () =
  Hashtbl.fold
    (fun _ v acc ->
       match v with
         | Builtin _ -> acc
         | Plugin(e, _) -> e :: acc)
    all []


let list_builtin () =
  let lst =
    Hashtbl.fold
      (fun _ v acc ->
         match v with
           | Builtin t -> t :: acc
           | Plugin(e, _) -> acc)
      all []
  in
    List.sort
      (fun scmd1 scmd2 -> String.compare scmd1.scmd_name scmd2.scmd_name)
      lst
