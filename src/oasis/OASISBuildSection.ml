
(** Build section functions
    @author Sylvain Le Gall
  *)

open OASISTypes

(* END EXPORT *)

open OASISSchema
open OASISValues
open OASISUtils
open OASISGettext

let build_depends_field schm = 
  new_field schm "BuildDepends" 
    ~default:[]
    (let base_value = 
       comma_separated 
         (with_optional_parentheses
            string_not_empty
            version_comparator)
     in
       {
         parse = 
           (fun str ->
              List.map 
                (fun (pkg, ver_constr_opt) -> 
                   FindlibPackage (pkg, ver_constr_opt))
                (base_value.parse str));
         print =
           (fun lst ->
              base_value.print
                (List.map 
                   (function 
                      | FindlibPackage (nm, ver) -> (nm, ver)
                      | InternalLibrary nm -> (nm, None))
                   lst));
       })
    (fun () -> s_ "Dependencies on findlib packages.")

let build_tools_field schm =
  new_field schm "BuildTools"
    ~default:[]
    (let base = 
       comma_separated file
     in
       {
         parse = 
           (fun str ->
              List.map 
                (fun s -> ExternalTool s) 
                (base.parse str));
         print =
           (fun lst ->
              base.print
                (List.map
                   (function
                      | InternalExecutable nm
                      | ExternalTool nm -> nm)
                lst))
       })
    (fun () -> s_ "Executables required to compile.")

let section_fields nm comp_dflt schm = 
  let path =
    new_field schm "Path" 
      directory
      (fun () ->
         Printf.sprintf
           (f_ "Directory containing the %s")
           nm)
  in
  let build = 
    new_field_conditional schm "Build"
      ~default:true
      boolean
      (fun () ->
         Printf.sprintf 
           (f_ "Set if the %s should be built. Use with flag.")
           nm)
  in
  let install =
    new_field_conditional schm "Install"
      ~default:true
      boolean
      (fun () ->
         Printf.sprintf
           (f_ "Set if the %s should be distributed.")
           nm)
  in
  let build_depends =
    build_depends_field schm
  in
  let build_tools =
    build_tools_field schm
  in
  let compiled_object =
    new_field schm "CompiledObject"
      ~default:comp_dflt
      (choices
         (fun () -> s_ "compiled object")
         ["byte", Byte; "native", Native; "best", Best])
      (fun () ->
         Printf.sprintf 
           (f_ "Define the compilation type of %s: byte, native or best")
           nm)
  in
  let c_sources = 
    new_field schm "CSources"
      ~default:[]
      files
      (fun () -> s_ "C source files.")
  in
  let data_files =
    new_field schm "DataFiles"
      ~default:[]
      (comma_separated
         (with_optional_parentheses
            (* TODO: these two strings are in fact "expendable strings" i.e. that
             * can contain $xxx, we need to check their correctness 
             *)
            string_not_empty
            string_not_empty))
      (fun () -> 
         s_ "Comma separated list of files to be installed for run-time use by \
             the package. Install by default in '$datadir/$pkg_name', you can \
             override using 'fn ($datadir/other_location)'. You can use \
             wildcard '*' but only for filename and followed by a single dot \
             extension: 'dir/*.html' is valid but 'dir/*' and 'dir/*.tar.gz' are \
             not valid.")
  in
  let ccopt = 
    new_field schm "CCOpt"
      ~default:[]
      space_separated
      (fun () ->
         s_ "-ccopt arguments to use when building.")
  in
  let cclib = 
    new_field schm "CCLib"
      ~default:[]
      space_separated
      (fun () ->
         s_ "-cclib arguments to use when building.")
  in
  let dlllib = 
    new_field schm "DllLib"
      ~default:[]
      space_separated
      (fun () ->
         s_ "-dlllib arguments to use when building.")
  in
  let dllpath = 
    new_field schm "DllPath"
      ~default:[]
      space_separated
      (fun () ->
         s_ "-dllpath arguments to use when building.")
  in
  let byteopt = 
    new_field schm "ByteOpt"
      ~default:[]
      space_separated
      (fun () ->
         s_ "ocamlc arguments to use when building.")
  in
  let nativeopt = 
    new_field schm "NativeOpt"
      ~default:[]
      space_separated
      (fun () ->
         s_ "ocamlopt arguments to use when building.")
  in
    (fun nm data ->
       {
         bs_build           = build data;
         bs_install         = install data;
         bs_path            = path data;
         bs_compiled_object = compiled_object data;
         bs_build_depends   = build_depends data;
         bs_build_tools     = build_tools data;
         bs_c_sources       = c_sources data;
         bs_data_files      = data_files data;
         bs_ccopt           = ccopt data;
         bs_cclib           = cclib data;
         bs_dlllib          = dlllib data;
         bs_dllpath         = dllpath data;
         bs_byteopt         = byteopt data;
         bs_nativeopt       = nativeopt data;
       })

(** {2 Graph of build depends}
  *)

open Graph
open OASISSection

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
         | Test (cs, tst) as sct ->
             let vrtx = 
               G.V.create sct
             in
               G.add_vertex g vrtx;
               add_build_tool 
                 vrtx
                 tst.test_build_tools
         | Flag _ | SrcRepo _ as sct ->
             G.add_vertex g (G.V.create sct))
      pkg.sections;

    g

let build_order ?graph pkg = 
  let g = 
    match graph with 
      | Some g -> g 
      | None -> build_graph pkg
  in
    Topological.fold 
      (fun v lst -> v :: lst)
      g
      []

module SetDepends = 
  Set.Make
    (struct
       type t = dependency
       let compare = compare
     end)

(** Returns a map between sections and its build depends.
    The build depends contains only libraries.
  *)
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
           | Flag _ | SrcRepo _ | Test _ as sct ->
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
             | Executable _ | Flag _ | SrcRepo _ | Test _ ->
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
        (build_order ~graph:g pkg)
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
              MapSectionId.find (KLibrary, nm) order
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

