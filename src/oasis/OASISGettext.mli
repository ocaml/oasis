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


(** i18n functions

    This module is really bind to ocaml-gettext library if gettext has been
    selected when compiling the project. All these functions {b are exported}
    but their are bound to {b dummy functions} in this case (i.e. not
    ocaml-gettext).

    @author Sylvain Le Gall
    @see <http://forge.ocamlcore.org/projects/ocaml-gettext> OCaml Gettext project
*)


(** Do nothing, but register it for translation *)
val ns_: string -> string


(** Translate a string. *)
val s_: string -> string


(** Translate a format string. *)
val f_: ('a, 'b, 'c, 'd) format4 -> ('a, 'b, 'c, 'd) format4


(** [fn_ fmt_singular fmt_plural n] Translate a plural string using either
    [fmt_singular] or [fmt_plural], depending of the plural status of number
    [n] in the target language.
*)
val fn_:
  ('a, 'b, 'c, 'd) format4 ->
  ('a, 'b, 'c, 'd) format4 ->
  int -> ('a, 'b, 'c, 'd) format4


(** Gettext initialization. *)
val init: (string * string option * string option) list
