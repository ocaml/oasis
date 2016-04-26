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


(** Define the command line arguments required for a subcommand. *)
type cli_parsing_t = (Arg.key * Arg.spec * Arg.doc) list * Arg.anon_fun

(** Function to run after parsing command line arguments. *)
type 'a cli_parsing_post_t = unit -> 'a

(** The main function to run the subcommand. *)
type 'a main_t = ctxt:OASISContext.t -> 'a

(** Generate command line arguments and the function to run the main of the
    subcommand.
*)
type 'a run_t = unit -> cli_parsing_t * 'a main_t

type t =
  {
    scmd_name: name;
    (** Name of the subcommand, used to call it *)

    scmd_synopsis: string;
    (** Short description of the subcommnad, displayed when doing a summary
        of the available subcommands
    *)

    scmd_help: string;
    (** Long description of the subcommand, displayed when showing help of the
        subcommand.

        It can contains variable substitution as defined in
        [Buffer.add_substitute].
    *)

    scmd_usage: string;
    (** [Arg] usage text *)

    scmd_deprecated: bool;
    (** Is this subcommand deprecated. *)

    scmd_run: unit run_t;
    (** Generate the specs and a main function. *)
  }

val default_anon: Arg.anon_fun
val default_fspecs: (unit -> cli_parsing_t * unit cli_parsing_post_t)

(** [make_run fspecs main] Create a [run_t] by combining argument parsing with
    the main function. The goal is to make [main] not use global variable and
    create what is need to store them in fspecs. This allow to make the whole
    subcommand invocation thread safe.
*)
val make_run:
  (unit -> cli_parsing_t * 'a cli_parsing_post_t) ->
  ('a -> 'b) main_t ->
  'b run_t

(** [register ~usage name synopsis help run] Create a subcommand using
    provided data, see {!t} for their meanings. See {!make_run} to define a run
    function. You can also see {!CLICommon} for useful functions to wrap your
    [run].
*)
val register:
  ?usage:string ->
  ?deprecated:bool ->
  name ->
  string ->
  string ->
  unit run_t ->
  unit

(** Find a subcommand. *)
val find: name -> t


(** List all PluginLoader entries registered. Set [deprecated] to false to
    filter out deprecated plugin
*)
val list_plugin: ?deprecated:bool -> unit -> PluginLoader.entry list

(** List all builtin plugin registered. Set [deprecated] to false to filter
    out deprecated builtin.
*)
val list_builtin: ?deprecated:bool -> unit -> t list
