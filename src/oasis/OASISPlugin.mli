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


(** Plugins creation and management

    The whole module is {b not exported}.

    @author Sylvain Le Gall
*)


(** {2 Types} *)


open OASISTypes


module MapPlugin: OASISUtils.MapExt.S with type key = plugin_kind plugin
module SetPlugin: OASISUtils.SetExt.S with type elt = plugin_kind plugin

(* Check that plugin exist even with a different version. *)
val mem_no_version: plugin_kind plugin -> SetPlugin.t -> bool


type 'a setter = plugin_data ref -> 'a -> unit
type 'a getter = plugin_data ref -> 'a
type 'a prop   = 'a setter * 'a getter


(** OCaml module embedded code.
*)
type modul = string


(** Describe setup file changes.
*)
type ('a, 'b) setup_changes =
  {
    chng_moduls: modul list;
    (** OCaml module to be added to setup file *)

    chng_main: 'a ODNFunc.func;
    (** Main function to be added to BaseSetup.t (i.e. the one that
        that really do something: configure, build, test...)
    *)

    chng_clean: 'b ODNFunc.func option;
    (** Function to be called when cleaning *)

    chng_distclean: 'b ODNFunc.func option;
    (** Function to be called when distcleaning *)
  }


(** Describe context when applying a plugin.
*)
type context_act =
  {
    ctxt: OASISContext.t;
    (** Global context. *)

    update: OASISSetupUpdate.t;
    (** What is the value given to -setup-update ? *)

    error: bool;
    (** Are there errors? *)

    files: OASISFileTemplate.templates;
    (** Generated files. *)

    other_actions: (unit -> unit) list;
    (** Extra actions. *)
  }


(** Generator for sections (document, test).
*)
type ('a, 'b) section_act =
  context_act ->
  package ->
  (common_section * 'a) ->

  (* Result *)
  context_act *

    ((* Run *)
      (package -> (common_section * 'a) -> string array -> 'b),

      (* Clean & Distclean *)
      (package -> (common_section * 'a) -> string array -> unit)
    ) setup_changes


(** Generator with a package argument only (build, install).
*)
type package_act =
  context_act ->
  package ->

  (* Result *)
  context_act *

    ((* Run *)
      (package -> string array -> unit),

      (* Clean & Distclean *)
      (package -> string array -> unit)
    ) setup_changes


(** Base types to build plugin: register fields, action, generators...
*)
type 'a t


(** Base types for all plugins
*)
type all_t = plugin_kind t


(** Register a quickstart completion for this plugin *)
val register_quickstart_completion: all_t -> (package -> package) -> unit


(** Get quickstart completion *)
val quickstart_completion: plugin_kind plugin -> package -> package


(** Register a generator for package, to store data of a plugin *)
val register_generator_package:
  all_t -> 'a prop -> (PropList.Data.t -> 'a) -> unit


(** Call generator for provided plugin *)
val generator_package:
  plugin_kind plugin -> plugin_data ref -> PropList.Data.t -> unit


(** Register a generator for a section, to store data of a plugin *)
val register_generator_section:
  section_kind -> all_t -> 'a prop -> (PropList.Data.t -> 'a) -> unit


(** Call generator for provided plugin on a section *)
val generator_section:
  section_kind -> plugin_kind plugin -> plugin_data ref ->
  PropList.Data.t -> unit


(** List available plugins. *)
val ls: plugin_kind -> name list


(* All registered plugin. *)
val all_plugins: unit -> plugin_kind plugin list


type help =
  {
    help_template:   string list;
    help_order:      int;
  }


val help_default: string list -> help


(** Register general help. We only rely on plugin name and version. The
    replacement field will be computed using the kind of the plugin.
*)
val register_help: [`All | plugin_kind] plugin -> help -> unit


(** Get general help text
*)
val help: [`All] plugin -> help


(** Convert back to plugin *)
val to_plugin: 'a t -> 'a plugin


(** Module to manage a set of plugins, of the same type. *)
module type PLUGINS =
sig
  type data
  type act
  type kind

  type self_t = kind t
  type self_plugin = kind plugin

  val create: self_plugin -> self_t * all_t

  (** Register the [section_act] or [package_act] datastructure. *)
  val register_act: self_t -> act -> unit

  (** Get action. *)
  val act: self_plugin -> act

  (** Quickstart question. *)
  val quickstart_question: unit -> self_plugin quickstart_question

  (** Parse a plugin field. *)
  val value: self_plugin OASISValues.t
end


(** {2 Modules for plugin type} *)


(** This module manage plugin that can handle configure step. *)
module Configure: PLUGINS with
  type act = package_act
                           and type data = package
                           and type kind = [`Configure]


(** This module manage plugin that can handle build step. *)
module Build: PLUGINS with
  type act = package_act
                       and type data = package
                       and type kind = [`Build]


(** This module manage plugin that can handle building documents. *)
module Doc: PLUGINS with
  type act = (doc, unit) section_act
                     and type data = common_section * doc
                     and type kind = [`Doc]


(** This module manage plugin that can handle running tests. *)
module Test: PLUGINS with
  type act = (test, float) section_act
                      and type data = common_section * test
                      and type kind = [`Test]


(** This module manage plugin that can handle install/uninstall steps. *)
module Install: PLUGINS with
  type act = package_act * package_act
                         and type data = package
                         and type kind = [`Install]


(** This module manage plugin that can handle configure step. *)
module Extra:     PLUGINS with
  type act = context_act -> package -> context_act
                           and type data = package
                           and type kind = [`Extra]


(** {2 General plugin functions} *)


(** Check that a field name has the form to match a plugin. Don't check that the
    plugin exists. This functions help to ignore plugin fields.
*)
val test_field_name: string -> bool


(** Use a builtin plugin (i.e. version = OASIS version). *)
val builtin: 'a -> name -> 'a plugin


(** Add a template to context *)
val add_file: OASISFileTemplate.template -> context_act -> context_act


(** Define an error in context. It doesn't stop processing, it just sets the
    {context_act.error} value.
*)
val set_error: bool -> string -> context_act -> context_act


(** Get a plugin from a string *)
val plugin_of_string: 'a -> string -> 'a plugin


(** Get a list of plugins from a string *)
val plugins_of_string: 'a -> string -> ('a plugin) list


(** Get a list of plugins from a string *)
val string_of_plugin: 'a plugin -> string


(** Compare plugin, caseless for name and don't take into account version
    if one is not set.
*)
val plugin_compare: 'a plugin -> 'a plugin -> int


(** Test equality for plugins, a special case of {!plugin_compare}.
*)
val plugin_equal: 'a plugin -> 'a plugin -> bool


(** Create storage for plugin data.
*)
val data_create: unit -> plugin_data ref


(** [data_new_property plg] Create a property that can store plugin data. Beware
    that the the couple [(plg, purpose)] must be unique.

    @param purpose An identifier to make possible the use of several properties
                 for the same plugin. If not defined, it is derived from the
                 kind of plugin.
*)
val data_new_property:
  ?purpose:plugin_data_purpose -> plugin_kind plugin -> 'a prop
