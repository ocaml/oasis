
(** Read-write access to 'setup.data'
    @author Sylvain Le Gall
  *)

open OASISTypes

(** Origin of the variable, if a variable has been already set
    with a higher origin, it won't be set again.
  *)
type origin_t = 
  | ODefault     (** Default computed value. *)
  | OGetEnv      (** Extracted from environment, using Sys.getenv. *)
  | OFileLoad    (** From loading file setup.data. *)
  | OCommandLine (** Set on command line. *)

(** Command line handling for variable.
  *)
type cli_handle_t =
  | CLINone
  (** No command line argument. *)
  | CLIAuto
  (** Build using variable name and help text. *)
  | CLIWith
  (** Use prefix --with-. *)
  | CLIEnable
  (** Use --enable/--disable. *)
  | CLIUser of (Arg.key * Arg.spec * Arg.doc) list
  (** Fully define the command line arguments. *)

(** Variable type.
  *)
type definition_t =
    {
      hide:       bool; (** Hide the variable. *)
      dump:       bool; (** Dump the variable. *)
      cli:        cli_handle_t;  (** Command line handling for the variable. *)
      arg_help:   string option; (** Help about the variable. *)
      group:      name option; (** Group of the variable. *)
    }

(** Schema for environment.
  *)
val schema : (origin_t, definition_t) PropList.Schema.t

(** Expand variable that can be found in string. Variable follow definition of
  * variable for [Buffer.add_substitute].
  *)
val var_expand : string -> string

(** Get variable.
  *)
val var_get : name -> string

(** Choose a value among conditional expressions.
  *)
val var_choose :
  ?printer:('a -> string) -> 
  ?name:string -> 
  'a OASISExpr.choices -> 
  'a

(** Protect a variable content, to avoid expansion.
  *)
val var_protect : string -> string

(** Define a variable.
  *)
val var_define :
  ?hide:bool ->
  ?dump:bool ->
  ?short_desc:(unit -> string) ->
  ?cli:cli_handle_t ->
  ?arg_help:string ->
  ?group:string -> 
  name ->
  string Lazy.t -> 
  (unit -> string)

(** Define a variable or redefine it.
  *)
val var_redefine :
  ?hide:bool ->
  ?dump:bool ->
  ?short_desc:(unit -> string) ->
  ?cli:cli_handle_t ->
  ?arg_help:string ->
  ?group:string ->
  name -> 
  string Lazy.t -> 
  (unit -> string)

(** Well-typed ignore for [var_define].
  *)
val var_ignore : (unit -> string) -> unit

(** Display all variables, even hidden one.
  *)
val print_hidden : unit -> string

(** Get all variables.
  *)
val var_all : unit -> name list

(** Environment default file.
  *)
val default_filename : host_filename

(** Initialize environment.
  *)
val load : ?allow_empty:bool -> ?filename:host_filename -> unit -> unit

(** Uninitialize environment.
  *)
val unload : unit -> unit

(** Save environment on disk.
  *)
val dump : ?filename:host_filename -> unit -> unit

(** Display environment to user.
  *)
val print : unit -> unit

(** Default command line arguments, computed using variable definitions. 
  *)
val args : unit -> (Arg.key * Arg.spec * Arg.doc) list
