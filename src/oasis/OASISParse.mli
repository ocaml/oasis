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


(** Parse '_oasis' file

    The whole module is {b not exported}.

    @author Sylvain Le Gall
*)


open OASISTypes

(** Default name of _oasis. *)
val default_oasis_fn: host_filename

(** [from_stream ~ctxt ~ignore_plugins ~fn st] Parse the OASIS file [~fn] and
    check it. If [~ignore_plugins] is set, ignore unknown plugin's fields in the
    file.
*)
val from_stream:
  ctxt:OASISContext.t ->
  ?fn:host_filename ->
  (char Stream.t) ->
  package


(** See {!from_stream}, apply to a filename.
*)
val from_file:
  ctxt:OASISContext.t ->
  host_filename ->
  package


(** See {!from_stream}, apply to a string.
*)
val from_string:
  ctxt:OASISContext.t ->
  ?fn:host_filename ->
  string ->
  package
