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


(** Register files built to be installed
    @author Sylvain Le Gall
*)


open OASISTypes


type t =
  | BExec    (* Executable. *)
  | BExecLib (* Library coming with executable. *)
  | BLib     (* Library. *)
  | BObj     (* Object. *)
  | BDoc     (* Document. *)


(** Register files built. Each files built is provided as a list
    of alternatives. At least one alternative file should exist
    when registering and we only register existing ones.
*)
val register: t -> name -> host_filename list list -> unit


(** Unregister all files built. *)
val unregister: t -> name -> unit


(** Fold-left files built, filter existing
    and non-existing files.
*)
val fold: t -> name -> ('a -> host_filename -> 'a) -> 'a -> 'a


(** Check if a library/object/doc/exec has been built.
*)
val is_built: t -> name -> bool


(** [of_executable loc_fn (cs, bs, exec)] Compute at the same time events
    that should be registered by {!register} and data returned by
    {!OASISExecutable.unix_exec_is}. Use [loc_fn], if generated files
    are moved to a directory different from sources (e.g. in directory
    "_build").
*)
val of_executable:
  (unix_filename -> host_filename) ->
  common_section * build_section *
    executable ->
  (t * name * host_filename list list) list *
    unix_filename * unix_filename option


(** [of_library loc_fn (cs, bs, lib)]  Same as {!of_executable}, but
    using {!OASISLibrary.generated_unix_files}.
*)
val of_library:
  (unix_filename -> host_filename) ->
  common_section * build_section * library ->
  (t * name * host_filename list list) list * unix_filename list list


(** [of_object loc_fn (cs, bs, lib)]  Same as {!of_executable}, but
    using {!OASISObject.generated_unix_files}.
*)
val of_object:
  (unix_filename -> host_filename) ->
  common_section * build_section * object_ ->
  (t * name * host_filename list list) list * unix_filename list list
