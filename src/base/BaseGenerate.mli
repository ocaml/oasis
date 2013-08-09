(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2008-2010, OCamlCore SARL                                    *)
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

(** Generate 'setup.ml' and other files
    @author Sylvain Le Gall
  *)

open OASISTypes

type update = NoUpdate | Weak | Dynamic

(** Restore generated files, when [generate] has been called with
    [~restore:true]. {b Not exported}.
  *)
val restore : ?msg:OASISContext.t -> unit -> unit

(** Generate 'setup.ml' file and the rest of the build system.
    {b Not exported}.
  *)
val generate :
  ?msg:OASISContext.t ->
  restore:bool ->
  backup:bool ->
  setup_fn:host_filename ->
  ?oasis_exec:host_filename ->
  ?oasis_fn:host_filename ->
  ?oasis_setup_args:string list ->
  update ->
  package ->
  OASISFileTemplate.file_generate_change list
