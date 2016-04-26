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


type vertex = int


module SetInt =
  Set.Make
    (struct
      type t = int
      let compare = ( - )
    end)


type 'a t =
  {
    mutable vertexes: ('a * SetInt.t ref) array;
    values: ('a, int) Hashtbl.t;
  }


let create len =
  {
    vertexes = [||];
    values   = Hashtbl.create len;
  }


let copy t =
  {
    vertexes = Array.copy t.vertexes;
    values   = Hashtbl.copy t.values;
  }


let value_of_vertex t v =
  if 0 <= v && v < Array.length t.vertexes then
    fst (Array.unsafe_get t.vertexes v)
  else
    invalid_arg "get_vertex"


let vertex_of_value t e =
  Hashtbl.find t.values e


let add_vertex t e =
  if Hashtbl.mem t.values e then
    Hashtbl.find t.values e
  else
    begin
      let v = Array.length t.vertexes in
      let nvertexes =
        Array.init
          (v + 1)
          (fun i ->
             if i = v then
               e, ref SetInt.empty
             else
               t.vertexes.(i))
      in
      t.vertexes <- nvertexes;
      Hashtbl.add t.values e v;
      v
    end


let add_edge t v1 v2 =
  let size = Array.length t.vertexes in
  if 0 <= v1 && v1 < size &&
     0 <= v2 && v2 < size then
    begin
      let _, edges = t.vertexes.(v1) in
      edges := SetInt.add v2 !edges
    end
  else
    invalid_arg "add_edge"


let topological_sort t =
  let size = Array.length t.vertexes in

  (* Empty list that will contain the sorted vertexes *)
  let l = ref [] in

  (* Visited vertexes *)
  let visited = Array.make size false in

  let reverted_edges =
    let arr = Array.make size [] in
    for v1 = 0 to size - 1 do
      SetInt.iter
        (fun v2 -> arr.(v2) <- v1 :: arr.(v2))
        !(snd t.vertexes.(v1))
    done;
    arr
  in

  let rec visit v =
    if not visited.(v) then
      begin
        visited.(v) <- true;
        List.iter visit reverted_edges.(v);
        l := v :: !l
      end
  in

  (* Go through all vertexes with no outgoing edges *)
  for v = 0 to size - 1 do
    visit v
  done;
  !l


let fold_edges f t acc =
  let racc = ref acc in
  for v1 = 0 to Array.length t.vertexes - 1 do
    SetInt.iter
      (fun v2 -> racc := f v1 v2 !racc)
      !(snd t.vertexes.(v1))
  done;
  !racc


let transitive_closure t =
  let size = Array.length t.vertexes in
  let visited = Array.make size false in

  let rec visit set v =
    if not visited.(v) then begin
      let () = visited.(v) <- true in
      (* The set of outgoing edges is not complete *)
      let current_set = snd t.vertexes.(v) in
      let set' =
        SetInt.fold
          (fun v set' -> visit set' v)
          !current_set !current_set
      in
      current_set := set';
      SetInt.union set set'
    end else begin
      (* The set is complete *)
      SetInt.union set !(snd t.vertexes.(v))
    end
  in

  for v = 0 to size - 1 do
    let _set: SetInt.t = visit SetInt.empty v in
    ()
  done
