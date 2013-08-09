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

(** Executable section
    @author Sylvain Le Gall
  *)

open OASISTypes

(** [unix_exec_is (cs, bs, exec) is_native ext_dll suffix_program] Compute the
    filename of the real executable, with full unix path. Also return executable
    library, if one exists (it happens when building bytecode executable with C
    stubs).
 *)
val unix_exec_is :
  common_section * build_section * executable ->
  (unit -> bool) ->
  (unit -> string) -> (unit -> string) ->
  unix_filename * unix_filename option

(** Schema for the section. {b Not exported}.
  *)
val schema : (common_section * build_section * executable) OASISSchema.t
