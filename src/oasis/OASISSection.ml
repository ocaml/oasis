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

(** Manipulate section 
    @author Sylvain Le Gall
  *)

open OASISTypes

type section_kind =
  | KLibrary 
  | KExecutable
  | KFlag
  | KSrcRepo
  | KTest
  | KDoc

(** Extract generic information 
  *)
let section_kind_common = 
  function
    | Library (cs, _, _) -> 
        KLibrary, cs
    | Executable (cs, _, _) ->
        KExecutable, cs
    | Flag (cs, _) ->
        KFlag, cs
    | SrcRepo (cs, _) ->
        KSrcRepo, cs
    | Test (cs, _) ->
        KTest, cs
    | Doc (cs, _) ->
        KDoc, cs

(** Common section of a section
  *)
let section_common sct =
  snd (section_kind_common sct)

(** Key used to identify section
  *)
let section_id sct = 
  let k, cs = 
    section_kind_common sct
  in
    k, cs.cs_name

let string_of_section sct =
  let k, nm =
    section_id sct
  in
    (match k with
       | KLibrary    -> "library" 
       | KExecutable -> "executable"
       | KFlag       -> "flag"
       | KSrcRepo    -> "src repository"
       | KTest       -> "test"
       | KDoc        -> "doc")
    ^" "^nm

(* END EXPORT *)

let section_fields _ _ =
  fun nm data ->
    {
      cs_name = nm;
      cs_data = data;
    }

(** {2 Module for full section} *)

(** Comparable section, we only rely on section_id
   for comparison
  *)
module CSection =
struct
  type t = section

  let id = section_id

  let compare t1 t2 = 
    compare (id t1) (id t2)
    
  let equal t1 t2 =
    (id t1) = (id t2)

  let hash t =
    Hashtbl.hash (id t)
end

module MapSection = Map.Make(CSection)
module SetSection = Set.Make(CSection)

(** {2 Module for id-only section} *)

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


(** {2 Search list with id} *)

let section_find id scts =
  List.find
    (fun sct -> id = section_id sct)
    scts

