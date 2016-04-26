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


(** Custom command to run before/after specific actions
    @author Sylvain Le Gall
*)


(* END EXPORT *)


open OASISSchema_intern
open OASISValues
open OASISTypes


let add_fields schm nm hlp_pre hlp_post sync =
  let pre_command =
    new_field_conditional schm ("Pre"^nm^"Command")
      ~default:None
      (opt command_line)
      hlp_pre
      (fun pkg -> (sync pkg).pre_command)
  in
  let post_command =
    new_field_conditional schm ("Post"^nm^"Command")
      ~default:None
      (opt command_line)
      hlp_post
      (fun pkg -> (sync pkg).post_command)
  in
  (fun data ->
     {
       pre_command  = pre_command data;
       post_command = post_command data;
     })

