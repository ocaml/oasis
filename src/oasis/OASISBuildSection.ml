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


(* END EXPORT *)


open OASISTypes
open OASISUtils
open OASISSection
open OASISSection_intern

module G = OASISGraph


type extended_kind = [section_kind | `ExternalTool | `FindlibPackage]
type vertex = extended_kind * string


let build_graph pkg =
  let g: vertex G.t = G.create (2 * List.length pkg.sections) in
  let sct_of_vrtx = Hashtbl.create (List.length pkg.sections) in
  let ext_of_vrtx = Hashtbl.create 13 in

  let sections =
    (* Start by creating all vertexes, because we will need it
     * to create edges.
    *)
    List.fold_left
      (fun acc sct ->
         let vrtx =
           G.add_vertex g ((OASISSection.section_id sct) :> vertex)
         in
         Hashtbl.add sct_of_vrtx vrtx sct;
         (vrtx, sct) :: acc)
      []
      pkg.sections
  in

  let add_build_tool vrtx lst =
    List.iter
      (function
        | InternalExecutable nm ->
          let dvrtx = G.vertex_of_value g (`Executable, nm) in
          G.add_edge g vrtx dvrtx
        | ExternalTool prog ->
          let dvrtx = G.add_vertex g (`ExternalTool, prog) in
          Hashtbl.add ext_of_vrtx dvrtx (`ExternalTool prog);
          G.add_edge g vrtx dvrtx)
      lst
  in

  let add_build_section vrtx bs =
    add_build_tool vrtx bs.bs_build_tools;
    List.iter
      (function
        | InternalLibrary nm ->
          let dvrtx =
            try G.vertex_of_value g (`Library, nm)
            with Not_found -> G.vertex_of_value g (`Object, nm) in
          G.add_edge g vrtx dvrtx
        | FindlibPackage (fndlb_nm, ver_opt) ->
          let dvrtx = G.add_vertex g (`FindlibPackage, fndlb_nm) in
          Hashtbl.add ext_of_vrtx dvrtx
            (`FindlibPackage (fndlb_nm, ver_opt));
          G.add_edge g vrtx dvrtx)
      bs.bs_build_depends
  in

  (* Add all edges. *)
  List.iter
    (fun (vrtx, sct) ->
       match sct with
         | Library (cs, bs, _)
         | Object (cs, bs, _)
         | Executable (cs, bs, _) ->
           add_build_section vrtx bs
         | Test (cs, {test_tools = build_tools})
         | Doc (cs, {doc_build_tools = build_tools}) ->
           add_build_tool vrtx build_tools
         | Flag _ | SrcRepo _ ->
           ())
    sections;

  sct_of_vrtx, ext_of_vrtx, g


let build_order pkg =
  let sct_of_vrtx, _, g = build_graph pkg in
  List.rev
    (List.fold_left
       (fun acc vrtx ->
          try
            Hashtbl.find sct_of_vrtx vrtx :: acc
          with Not_found ->
            acc)
       []
       (G.topological_sort g))


let transitive_build_depends pkg =
  let sct_of_vrtx, ext_of_vrtx, g = build_graph pkg in

  let order =
    (* Map depends with their build order. *)
    let hshtbl = Hashtbl.create 13 in
    let idx = ref 0 in
    List.iter
      (fun dep ->
         Hashtbl.add hshtbl dep !idx;
         incr idx)
      (G.topological_sort g);
    hshtbl
  in

  let map_deps =
    (* Fill the map with empty depends *)
    List.fold_left
      (fun mp sct -> MapSection.add sct [] mp)
      MapSection.empty
      pkg.sections
  in

  let map_deps =
    let add_dep sct dep mp =
      let lst = try MapSection.find sct mp with Not_found -> [] in
      MapSection.add sct (dep :: lst) mp
    in
    let g' = G.copy g in
    G.transitive_closure g';
    G.fold_edges
      (fun vrtx1 vrtx2 mp ->
         if Hashtbl.mem sct_of_vrtx vrtx1 then
           begin
             let sct = Hashtbl.find sct_of_vrtx vrtx1 in
             let ord = Hashtbl.find order vrtx2 in
             match G.value_of_vertex g' vrtx2 with
               | `Library, nm ->
                 add_dep sct (ord, InternalLibrary nm) mp
               | `FindlibPackage, _ ->
                 begin
                   match Hashtbl.find ext_of_vrtx vrtx2 with
                     | `FindlibPackage (fndlb_nm, ver_opt) ->
                       add_dep
                         sct
                         (ord, FindlibPackage (fndlb_nm, ver_opt))
                         mp
                     | _ ->
                       mp
                 end
               | _ ->
                 mp
           end
         else
           mp)
      g'
      map_deps
  in

  MapSection.mapi
    (fun k lst ->
       List.rev_map
         (fun (_, dep) -> dep)
         (* Reverse order to match List.rev_map *)
         (List.sort (fun (o1, _) (o2, _) -> o2 - o1) lst))
    map_deps
