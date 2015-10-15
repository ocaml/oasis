(** Search source files, using pattern and templates.
    @author Sylvain Le Gall
    @since 0.4.7

    See `_oasis` fields 'InterfacePatterns' and 'ImplementationPatterns'.
  *)
open OASISUnixPath

(** A template to find a file. *)
type t

(** List of templates to search an implementation file matching a module. *)
val implementation: t list

(** List of templates to search an interface file metching a module. *)
val interface: t list

(** Parse the given string. *)
val parse: string -> t

(** Return the string representation of [t]. *)
val to_string: t -> string

(** List all possible files using the list of templates.

    @params modul The module name as defined in the field "Modules" and
    "InternalModules"
    @params path The base path of the module as defined in "Path"
    @raise Not_found if no templates match the given module
    @return The filename of the first matching template.
  *)
val all_possible_files:
  t list -> path:unix_dirname -> modul:string -> unix_filename list

(** Convert to OASISDataNotation. *)
val odn_of_t: t -> OASISDataNotation.t

