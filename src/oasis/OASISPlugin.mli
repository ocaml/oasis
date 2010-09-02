
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
  type t

  (** Register the [section_act] or [package_act] datastructure. *)
  val register : t -> unit

  (** Create a field for this plugin. *)
  val new_field :
    OASISSchema.t ->
    name ->
    ?default:'a ->
    'a OASISValues.t -> (unit -> string) -> PropList.Data.t -> 'a

  (** Create a conditional field for this plugin. *)
  val new_field_conditional :
    OASISSchema.t ->
    name ->
    ?default:'a ->
    'a OASISValues.t ->
    (unit -> string) -> PropList.Data.t -> 'a OASISExpr.choices
end

(** Plugin identification *)
module type PLUGIN_ID_TYPE =
sig
  (** Name of the plugin. *)
  val name : string

  (** Version of the plugin. *)
  val version : string

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
  type t

  (** Create {PLUGIN_UTILS_TYPE} for a {PLUGIN_ID_TYPE}. *)
  module Make: functor (PI : PLUGIN_ID_TYPE) -> PLUGIN_UTILS_TYPE with type t = t

  (** List available plugins. *)
  val ls : unit -> name list

  (** Find a specific plugin. *)
  val find : name * 'a -> t

  (** Parse a plugin field *) 
  val value : (name * OASISVersion.t option) OASISValues.t
end

module MapPlugin: Map.S with type key = name

(** {2 Modules for plugin type} *)

(** This module manage plugin that can handle configure step. *)
module Configure: PLUGINS with type t = package_act

(** This module manage plugin that can handle build step. *)
module Build:     PLUGINS with type t = package_act

(** This module manage plugin that can handle building documents. *)
module Doc:       PLUGINS with type t = (doc, unit) section_act

(** This module manage plugin that can handle running tests. *)
module Test:      PLUGINS with type t = (test, float) section_act

(** This module manage plugin that can handle install/uninstall steps. *)
module Install:   PLUGINS with type t = package_act * package_act

(** This module manage plugin that can handle configure step. *)
module Extra:     PLUGINS with type t = context_act -> package -> context_act

(** {2 General plugin functions} *)

(* General data for plugin. *)
(* TODO: don't expose the ref *)
val help_all :
  (string * int * string list * (string * (unit -> string)) list) MapPlugin.t
  ref

(** Check that a field name has the form to match a plugin. Don't check that the 
    plugin exists. This functions help to ignore plugin fields.
  *)
val test_field_name : string -> bool

(** Use a builtin plugin (i.e. version = OASIS version). *)
val builtin : name -> name * OASISVersion.t option

(** Add a template to context *)
val add_file : OASISFileTemplate.template -> context_act -> context_act

(** Define an error in context. It doesn't stop processing, it just sets the
    {context_act.error} value.
  *)
val set_error : bool -> string -> context_act -> context_act
