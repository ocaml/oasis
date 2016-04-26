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


let section_kind_common =
  function
    | Library (cs, _, _) ->
      `Library, cs
    | Object (cs, _, _) ->
      `Object, cs
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


let section_common sct =
  snd (section_kind_common sct)


let section_common_set cs =
  function
    | Library (_, bs, lib)     -> Library (cs, bs, lib)
    | Object (_, bs, obj)      -> Object (cs, bs, obj)
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
    | `Library    -> "library"
    | `Object     -> "object"
    | `Executable -> "executable"
    | `Flag       -> "flag"
    | `SrcRepo    -> "src repository"
    | `Test       -> "test"
    | `Doc        -> "doc")
  ^" "^nm


let section_find id scts =
  List.find
    (fun sct -> id = section_id sct)
    scts


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


(* END EXPORT *)
