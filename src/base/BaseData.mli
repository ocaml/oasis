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


(** Exported modules for embedding

    The whole module is {b not exported}. It is auto-generated using other
    modules.
*)


(** All exported modules from base library, default
    content for 'setup.ml'.
*)
val basesys_ml: string


(** Minimal set of exported modules to load the 'setup.data'
    files. Use to create OCaml script that will use 'setup.data'.
    Example auto-generated 'myocamlbuild.ml' contains this set.
*)
val basesysenvironment_ml: string


(** Set of modules to load for the 'bundle' subcommand
*)
val basesysbundle_ml: string


(** Toploop for dynrun. *)
val dynrun_ml: string


(** Toploop for dynrun (alternative). *)
val dynrun_for_release_ml: string


(** Toploop for dynrun (yet another alternative). *)
val compiled_setup_ml: string
