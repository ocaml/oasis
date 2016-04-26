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

type cli_parsing_t = (Arg.key * Arg.spec * Arg.doc) list * Arg.anon_fun
type 'a cli_parsing_post_t = unit -> 'a
type 'a main_t = ctxt:OASISContext.t -> 'a
type 'a run_t = unit -> cli_parsing_t * 'a main_t

type t =
  {
    scmd_name: string;
    scmd_synopsis: string;
    scmd_help: string;
    scmd_usage: string;
    scmd_deprecated: bool;
    scmd_run: unit run_t;
  }

type registered_t = Builtin of t | Plugin of (PluginLoader.entry * t option)


(** Default values for anon and specs. *)
let default_usage = ns_ "[options*]"
let default_anon = failwithf (f_ "Don't know what to do with '%s'.")
let default_fspecs () = ([], default_anon), (fun () -> ())


let make_run
    (fspecs: unit -> cli_parsing_t * 'a cli_parsing_post_t)
    (main: ('a -> 'b) main_t) () =
  let cli_parsing, cli_parsing_post = fspecs () in
  let main' ~ctxt =
    let a = cli_parsing_post () in
    main ~ctxt a
  in
  cli_parsing, main'


(* TODO: protect with mutex. *)
let all = Hashtbl.create 20
(* TODO: protect with synchronisation. *)
let loading_plugin = ref None


let () =
  PluginLoader.init CLIPluginsLoaded.exec_oasis_build_depends_rec;
  List.iter
    (fun e ->
       Hashtbl.add all e.PluginLoader.name (Plugin(e, None)))
    (PluginLoader.list (plugin_cli_t ()))


let register ?(usage=default_usage) ?(deprecated=false) nm synopsis help run =
  let merge_option opt txt =
    match opt with
      | Some txt -> txt
      | None -> txt
  in
  let t =
    {
      scmd_name = nm;
      scmd_synopsis = synopsis;
      scmd_help = help;
      scmd_usage = usage;
      scmd_deprecated = deprecated;
      scmd_run = run;
    }
  in
  try
    match Hashtbl.find all t.scmd_name with
      | Plugin (e, None) ->
        let t' =
          {t with
             scmd_synopsis = merge_option
                 e.PluginLoader.synopsis
                 t.scmd_synopsis;
             scmd_deprecated = e.PluginLoader.deprecated || t.scmd_deprecated}
        in
        Hashtbl.replace all t.scmd_name (Plugin(e, Some t'))
      | Builtin _ ->
        failwithf
          (f_ "Trying to double-register the builtin subcommand %s.")
          t.scmd_name
      | Plugin (e, Some _) ->
        failwithf
          (f_ "Trying to double-register the plugin subcommand %s (%s).")
          nm (e.PluginLoader.findlib_name)

  with Not_found ->
  match !loading_plugin with
    | None ->
      Hashtbl.add all t.scmd_name (Builtin t)
    | Some findlib_name ->
      failwithf
        (f_ "Trying to register unknown plugin subcommand %s within loading
                 of %s.")
        nm findlib_name


let find nm =
  let load_plugin nm =
    match !loading_plugin with
      | Some nm' ->
        failwithf
          (f_ "Recursive loading of plugins (%s and %s).")
          nm nm'
      | None ->
        loading_plugin := Some nm;
        PluginLoader.load (plugin_cli_t ()) nm;
        loading_plugin := None
  in
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
          load_plugin nm;
          find' true
    with Not_found ->
      failwithf (f_ "Subcommand '%s' doesn't exist") nm
  in
  find' false


let list_plugin ?(deprecated=true) () =
  Hashtbl.fold
    (fun _ v acc ->
       match v with
         | Builtin _ -> acc
         | Plugin(e, _) ->
           if deprecated || not e.PluginLoader.deprecated then
             e :: acc
           else
             acc)
    all []


let list_builtin ?(deprecated=true) () =
  let lst =
    Hashtbl.fold
      (fun _ v acc ->
         match v with
           | Builtin t ->
             if deprecated || not t.scmd_deprecated then
               t :: acc
             else
               acc
           | Plugin(e, _) -> acc)
      all []
  in
  lst
