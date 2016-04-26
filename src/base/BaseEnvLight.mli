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


(** Read-only access to 'setup.data'

    This module defines the minimal set of functions to access data
    contained in 'setup.data'. It allows to build third party OCaml
    script, without embedding hundreeds line of code.

    @author Sylvain Le Gall
*)


module MapString: Map.S with type key = string


type t = string MapString.t


(** Environment default file
*)
val default_filename: string Lazy.t


(** Load environment.
*)
val load: ?allow_empty:bool -> ?filename:string -> unit -> t

(** Expand a variable, replacing $(X) by variable X recursively.
*)
val var_expand: string -> t -> string

(** Get a variable that evaluate expression that can be found in it (see
    [Buffer.add_substitute]).
*)
val var_get: string -> t -> string


(** Choose a value among conditional expressions.
*)
val var_choose: 'a OASISExpr.choices -> t -> 'a
