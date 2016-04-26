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


(** Messages to user

    These functions display information depending on the verbosity level
    set in {!OASISContext.t}. They use [Printf.fprintf] syntax to output.
    You can use a [~after] function, that will be called if something has
    been displayed.

    @author Sylvain Le Gall
*)

(** Generic message.
*)
val generic_message:
  ctxt:OASISContext.t ->
  [`Debug|`Info|`Warning|`Error] ->
  ('a, unit, string, unit) format4 -> 'a


(** Print a debug message.
*)
val debug: ctxt:OASISContext.t -> ('a, unit, string, unit) format4 -> 'a


(** Print information message.
*)
val info: ctxt:OASISContext.t -> ('a, unit, string, unit) format4 -> 'a


(** Print a warning message.
*)
val warning: ctxt:OASISContext.t -> ('a, unit, string, unit) format4 -> 'a


(** Print an error message.
*)
val error: ctxt:OASISContext.t -> ('a, unit, string, unit) format4 -> 'a

