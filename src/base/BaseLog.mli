(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2016, Sylvain Le Gall                                   *)
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


(** Maintain a log of actions done
    @author Sylvain Le Gall
*)


open OASISTypes


(** Default file for registering log.
*)
val default_filename: OASISContext.source_filename


(** Load the log file.
*)
val load: ctxt:OASISContext.t -> unit -> (name * string) list


(** Add an event to the log file.
*)
val register: ctxt:OASISContext.t ->  name -> string -> unit


(** Remove an event from the log file.
*)
val unregister: ctxt:OASISContext.t ->  name -> string -> unit


(** Filter events of the log file.
*)
val filter: ctxt:OASISContext.t ->  name list -> (name * string) list


(** Check if an event exists in the log file.
*)
val exists: ctxt:OASISContext.t ->  name -> string -> bool
