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

(* END EXPORT *)

open OASISTypes
open OASISUtils

open Graph
open OASISSection
open OASISSection_intern

module G = Imperative.Digraph.Concrete(OASISSection.CSection)
module Bfs = Traverse.Bfs(G)
module Dfs = Traverse.Dfs(G)
module Topological = Topological.Make(G)
module Oper = Oper.I(G)

module Display =
struct
  include G
  let vertex_name v = varname_of_string (string_of_section v)
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end

module Dot = Graphviz.Dot(Display)

let show g =
  let tmp = Filename.temp_file "graph" ".dot" in
  let oc = open_out tmp in
  Dot.output_graph oc g;
  close_out oc;
  ignore (Sys.command ("dot -Tps " ^ tmp ^ " | gv -"));
  Sys.remove tmp

let build_graph pkg =
  let g =
    G.create ()
  in
  let vertex_of_section f =
    List.fold_left
      (fun acc sct ->
         match f sct with
           | Some cs ->
               MapString.add cs.cs_name (G.V.create sct) acc
           | None ->
               acc)
      MapString.empty
      pkg.sections
  in
  let vertex_of_lib =
    vertex_of_section
      (function Library (cs, _, _) -> Some cs | _  -> None)
  in
  let vertex_of_exec =
    vertex_of_section
      (function Executable (cs, _, _) -> Some cs | _ -> None)
  in
  let find_name nm mp =
    MapString.find nm mp
  in
  let add_build_tool vrtx lst =
    List.iter
      (function
         | InternalExecutable nm ->
             let dvrtx =
               find_name nm vertex_of_exec
             in
               G.add_edge g vrtx dvrtx
         | ExternalTool _ ->
             ())
      lst
  in
  let add_build_section vrtx bs =
    G.add_vertex g vrtx;
    add_build_tool vrtx bs.bs_build_tools;
    List.iter
      (function
         | InternalLibrary nm ->
             let dvrtx =
               find_name nm vertex_of_lib
             in
               G.add_edge g vrtx dvrtx
         | FindlibPackage _ ->
             ())
      bs.bs_build_depends
  in

    List.iter
      (function
         | Library (cs, bs, _) ->
             add_build_section
               (find_name cs.cs_name vertex_of_lib)
               bs
         | Executable (cs, bs, _) ->
             add_build_section
               (find_name cs.cs_name vertex_of_exec)
               bs
         | Test (cs, {test_tools = build_tools})
         | Doc (cs, {doc_build_tools = build_tools}) as sct ->
             let vrtx =
               G.V.create sct
             in
               G.add_vertex g vrtx;
               add_build_tool
                 vrtx
                 build_tools
         | Flag _ | SrcRepo _ as sct ->
             G.add_vertex g (G.V.create sct))
      pkg.sections;

    g

let build_order pkg =
  Topological.fold
    (fun v lst -> v :: lst)
    (build_graph pkg)
    []

module SetDepends =
  Set.Make
    (struct
       type t = dependency
       let compare = compare
     end)

let transitive_build_depends pkg =
  let g =
    build_graph pkg
  in

  let add_build_depends =
    List.fold_left
      (fun acc dep -> SetDepends.add dep acc)
  in

  let map_deps =
    (* Fill the map with empty depends *)
    List.fold_left
      (fun mp ->
         function
           | Library (_, bs, _) | Executable (_, bs, _) as sct ->
               MapSection.add
                 sct
                 (add_build_depends
                    SetDepends.empty
                    bs.bs_build_depends)
                 mp
           | Flag _ | SrcRepo _ | Test _ | Doc _ as sct ->
               MapSection.add sct SetDepends.empty mp)
      MapSection.empty
      pkg.sections
  in
  let map_deps =
    (* Populate build depends *)
    G.fold_edges
      (fun v1 v2 mp ->
         let deps =
           MapSection.find v1 mp
         in
         let deps =
           match v2 with
             | Library (cs, bs, _) ->
                 add_build_depends
                   deps
                   bs.bs_build_depends
             | Executable _ | Flag _ | SrcRepo _ | Test _ | Doc _ ->
                 deps
         in
           MapSection.add v1 deps mp)
      (Oper.transitive_closure g)
      map_deps
  in

  let extract_depends =
    let _, order =
      List.fold_left
        (fun (i, mp) sct ->
           i + 1,
           MapSectionId.add
             (section_id sct)
             i
             mp)
        (0, MapSectionId.empty)
        (build_order pkg)
    in

    let compare dep1 dep2 =
      let id =
        function
          | FindlibPackage _ ->
              (* We place findlib package at the very
               * beginning of the list, since they are
               * don't depend on any internal libraries
               * and that their inter-dependencies will
               * be solved by findlib
               *)
              (-1)
          | InternalLibrary nm ->
              MapSectionId.find (`Library, nm) order
      in
        (id dep1) - (id dep2)
    in
      fun k deps ->
        let lst =
          List.sort
            compare
            (SetDepends.elements deps)
        in
          lst
  in

    MapSection.mapi extract_depends map_deps

