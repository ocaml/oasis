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


(** Global context for messages and i18n
    @author Sylvain Le Gall
*)


type level =
  [ `Debug
  | `Info
  | `Warning
  | `Error]


type t =
  {
    quiet: bool;
    (** Display nothing. *)

    info: bool;
    (** Display info messages. *)

    debug: bool;
    (** Display also debug messages. *)

    ignore_plugins: bool;
    (** Don't use plugins *)

    ignore_unknown_fields: bool;
    (** Ignore fields unknown *)

    printf: level -> string -> unit;
    (** Redirect output *)
  }


(** Default context *)
val default: t ref


(** Command line arguments to create {!t}. *)
val fspecs: unit -> ((string * Arg.spec * string) list * (unit -> t))


(** Quiet context. *)
val quiet: t
