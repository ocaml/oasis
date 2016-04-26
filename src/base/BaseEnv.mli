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
val schema: (origin_t, definition_t) PropList.Schema.t


(** Data for environment.
*)
val env: PropList.Data.t


(** Expand variable that can be found in string. Variable follow definition of
  * variable for [Buffer.add_substitute].
*)
val var_expand: string -> string


(** Get variable.
*)
val var_get: name -> string


(** Choose a value among conditional expressions.
*)
val var_choose:
  ?printer:('a -> string) ->
  ?name:string ->
  'a OASISExpr.choices ->
  'a


(** Protect a variable content, to avoid expansion.
*)
val var_protect: string -> string


(** Define a variable.
*)
val var_define:
  ?hide:bool ->
  ?dump:bool ->
  ?short_desc:(unit -> string) ->
  ?cli:cli_handle_t ->
  ?arg_help:string ->
  ?group:string ->
  name ->
  (unit -> string) ->
  (unit -> string)


(** Define a variable or redefine it.
*)
val var_redefine:
  ?hide:bool ->
  ?dump:bool ->
  ?short_desc:(unit -> string) ->
  ?cli:cli_handle_t ->
  ?arg_help:string ->
  ?group:string ->
  name ->
  (unit -> string) ->
  (unit -> string)


(** Well-typed ignore for [var_define].
*)
val var_ignore: (unit -> string) -> unit


(** Display all variables, even hidden one.
*)
val print_hidden: unit -> string


(** Get all variables.
*)
val var_all: unit -> name list


(** Environment default file.
*)
val default_filename: host_filename Lazy.t


(** Initialize environment.
*)
val load: ?allow_empty:bool -> ?filename:host_filename -> unit -> unit


(** Uninitialize environment.
*)
val unload: unit -> unit


(** Save environment on disk.
*)
val dump: ?filename:host_filename -> unit -> unit


(** Display environment to user.
*)
val print: unit -> unit


(** Default command line arguments, computed using variable definitions.
*)
val args: unit -> (Arg.key * Arg.spec * Arg.doc) list
