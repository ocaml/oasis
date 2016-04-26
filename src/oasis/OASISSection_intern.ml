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


open OASISTypes
open OASISSection


(** Add section fields *)
let section_fields
    hlp
    (schm: 'a OASISSchema_intern.t)
    (sync: 'a -> common_section) =
  fun (features_data: OASISFeatures.Data.t) nm data ->
    {
      cs_name = nm;
      cs_plugin_data = []; (* TODO *)
      cs_data = data;
    }


(** {2 Containers for sections using id-only} *)


module CIdSection =
struct
  type t = section_kind * name
  let compare = compare
end


module MapSectionId = Map.Make(CIdSection)
module SetSectionId = Set.Make(CIdSection)


(** Convert a MapSection.t into a MapSectionId.t
*)
let map_section_id mp =
  MapSection.fold
    (fun k v mp ->
       MapSectionId.add
         (section_id k)
         v
         mp)
    mp
    MapSectionId.empty
