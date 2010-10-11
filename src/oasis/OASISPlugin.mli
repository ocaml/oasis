
(** Plugins creation and management
  
    The whole module is {b not exported}.

    @author Sylvain Le Gall
  *)


open OASISTypes

(** {2 Types} *)

(** OCaml module embedded code.
  *)
type modul = string

(** Describe setup file changes. 
  *)
type ('a, 'b) setup_changes = 
    {
      chng_moduls : modul list;
      (** OCaml module to be added to setup file *)

      chng_main : 'a ODNFunc.func;
      (** Main function to be added to BaseSetup.t (i.e. the one that 
          that really do something: configure, build, test...)
        *)

      chng_clean : 'b ODNFunc.func option;
      (** Function to be called when cleaning *)

      chng_distclean : 'b ODNFunc.func option;
      (** Function to be called when distcleaning *)
    }

(** Describe context when applying a plugin.
  *)
type context_act = 
    {
      ctxt : OASISContext.t;
      (** Global context. *)

      error : bool;
      (** Are there errors? *)

      files : OASISFileTemplate.templates;
      (** Generated files. *)

      other_actions : (unit -> unit) list;
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

(** Module to build plugins, provide functions automatically attached
    to a specific plugin. Example: {!new_field} will attach the field
    created to the plugin defined by a {PLUGIN_ID_TYPE} module.
  *)
module type PLUGIN_UTILS_TYPE =
sig
  type act
  type data

  (** Register the [section_act] or [package_act] datastructure. *)
  val register_act: act -> unit

  (** Register a quickstart completion for this plugin *)
  val register_quickstart_completion: (package -> package) -> unit

  (** Create a field for this plugin. *)
  val new_field :
    ('b OASISSchema.t) ->
    name ->
    ?default:'a ->
    'a OASISValues.t -> 
    (unit -> string) -> 
    PropList.Data.t -> 'a

  (** Create a conditional field for this plugin. *)
  val new_field_conditional :
    ('b OASISSchema.t) ->
    name ->
    ?default:'a ->
    'a OASISValues.t ->
    (unit -> string) -> 
    PropList.Data.t -> 'a OASISExpr.choices
end

(** Plugin identification *)
module type PLUGIN_ID_TYPE =
sig
  (** Name of the plugin. *)
  val name : string

  (** Version of the plugin. *)
  val version : OASISVersion.t

  (** Help text. It can contains variable substitution as defined
      in [Buffer.add_substitute].
    *)
  val help : string list

  (** Extra variables to substitute in {!help}. *)
  val help_extra_vars : (string * (unit -> string)) list

  (** Define help order with regard of other plugins. *)
  val help_order : int
end

(** Module to manage a set of plugins, of the same type. *)
module type PLUGINS =
sig
  type act
  type data
  type kind

  (** Create {PLUGIN_UTILS_TYPE} for a {PLUGIN_ID_TYPE}. *)
  module Make: functor (PI : PLUGIN_ID_TYPE) -> 
    PLUGIN_UTILS_TYPE with type act = act and type data = data

  (** List available plugins. *)
  val ls : unit -> name list

  (** Find a specific plugin. *)
  val find : kind plugin -> act

  (** Quickstart question *)
  val quickstart_question: 
    unit -> (kind plugin) quickstart_question

  (** Parse a plugin field *) 
  val value : (kind plugin) OASISValues.t

  (** Get quickstart completion *)
  val quickstart_completion: kind plugin -> package -> package
end

module MapPlugin: Map.S with type key = plugin_kind plugin
module SetPlugin: Set.S with type elt = plugin_kind plugin

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

(* General data for plugin. *)
val help : unit -> 
    (OASISVersion.t * int * string list * (string * (unit -> string)) list) MapPlugin.t

(** Check that a field name has the form to match a plugin. Don't check that the 
    plugin exists. This functions help to ignore plugin fields.
  *)
val test_field_name : string -> bool

(** Use a builtin plugin (i.e. version = OASIS version). *)
val builtin : 'a -> name -> 'a plugin

(** Add a template to context *)
val add_file : OASISFileTemplate.template -> context_act -> context_act

(** Define an error in context. It doesn't stop processing, it just sets the
    {context_act.error} value.
  *)
val set_error : bool -> string -> context_act -> context_act

(** Get a plugin from a string *)
val plugin_of_string: 'a -> string -> 'a plugin

(** Get a list of plugins from a string *)
val plugins_of_string: 'a -> string -> ('a plugin) list

(** Compare plugin, caseless for name and don't take into account version
    if one is not set.
  *)
val plugin_compare: 'a plugin -> 'a plugin -> int
