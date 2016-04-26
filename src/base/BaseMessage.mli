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


(** Message to user

    This module is the same as {!OASISMessage}, except we use the
    context {!BaseContext.default}.

    @author Sylvain Le Gall
*)


(** See {!OASISMessage.debug}.
*)
val debug: ('a, unit, string, unit) format4 -> 'a


(** See {!OASISMessage.info}.
*)
val info: ('a, unit, string, unit) format4 -> 'a


(** See {!OASISMessage.warning}.
*)
val warning: ('a, unit, string, unit) format4 -> 'a


(** See {!OASISMessage.error}.
*)
val error: ('a, unit, string, unit) format4 -> 'a
