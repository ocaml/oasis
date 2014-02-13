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


(** SubCommand definition
    @author Sylvain Le Gall
  *)


open OASISTypes


type t =
    {
      scmd_name:     name;   (** Name of the subcommand, used to call it *)
      scmd_synopsis: string; (** Short description of the subcommnad,
                                 displayed when doing a summary of the
                                 available subcommands
                               *)
      scmd_help:     string; (** Long description of the subcommand,
                                 displayed when showing help of the
                                 subcommand.

                                 It can contains variable substitution as
                                 defined in [Buffer.add_substitute].
                               *)
      scmd_specs:    (Arg.key * Arg.spec * Arg.doc) list;
                                     (** [Arg] spec list *)
      scmd_usage:    string;         (** [Arg] usage text *)
      scmd_anon:     string -> unit; (** [Arg] anon function *)
      scmd_main:     unit -> unit; (** Real action of the subcommand *)
    }


(** [make ~std_usage name synopsis help main] Create a subcommand using
    provided data, see {!t} for their meanings. If [~std_usage] is set
    use ["[options*]"] for it. Fields that are not defined by make use
    a sane default.
  *)
val make:
  ?std_usage:bool ->
  name ->
  string ->
  string ->
  (unit -> unit) ->
  t


(** Initialize. *)
val init: unit -> unit


(** Don't register builtin plugin past this point. *)
val freeze: unit -> unit

(** Register a builtin subcommand. This can only be done by builtin plugin and
    before freezing.
  *)
val register_builtin: t -> unit


(** Register a plugin subcommand. Synopsis is overriden by the plugin_synopsis
  * present in findlib if there is some defined.
  *)
val register_plugin: t -> unit


(** Find a subcommand.
  *)
val find: name -> t


(** List all PluginLoader entries registered. *)
val list_plugin: unit -> PluginLoader.entry list

(** List all builtin plugin registered. *)
val list_builtin: unit -> t list
