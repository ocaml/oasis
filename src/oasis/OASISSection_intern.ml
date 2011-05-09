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
        `Library, cs
    | Executable (cs, _, _) ->
        `Executable, cs
    | Flag (cs, _) ->
        `Flag, cs
    | SrcRepo (cs, _) ->
        `SrcRepo, cs
    | Test (cs, _) ->
        `Test, cs
    | Doc (cs, _) ->
        `Doc, cs

(** Common section of a section
  *)
let section_common sct =
  snd (section_kind_common sct)

(** Set the common part of a section 
  *)
let section_common_set cs =
  function
    | Library (_, bs, lib)     -> Library (cs, bs, lib)
    | Executable (_, bs, exec) -> Executable (cs, bs, exec)
    | Flag (_, flg)            -> Flag (cs, flg)
    | SrcRepo (_, src_repo)    -> SrcRepo (cs, src_repo)
    | Test (_, tst)            -> Test (cs, tst)
    | Doc (_, doc)             -> Doc (cs, doc)

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

let section_fields 
      hlp 
      (schm: 'a OASISSchema_intern.t)
      (sync: 'a -> common_section) =
  fun nm data ->
    {
      cs_name = nm;
      cs_plugin_data = []; (* TODO *)
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
