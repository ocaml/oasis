(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2008-2010, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

(** Flag schema and generator
    @author Sylvain Le Gall
  *)

open OASISTypes

(* END EXPORT *)

open OASISSchema
open OASISValues
open OASISUtils
open OASISGettext
open PropList.Field

let schema, generator = 
  let schm =
    schema "Flag" 
  in
  let cmn_section_gen =
    OASISSection.section_fields (s_ "flag") schm
  in
  let descr = 
    new_field schm "Description" 
      ~default:None 
      (opt string_not_empty)
      (fun () -> 
         s_ "Help for the flag")
  in
  let default = 
    new_field_conditional schm "Default" 
      ~default:true
      boolean
      (fun () ->
         s_ "Default value for the flag")
  in
    schm,
    (fun nm data ->
       Flag
         (cmn_section_gen nm data,
          {
            flag_description = descr data;
            flag_default     = default data;
          }))
