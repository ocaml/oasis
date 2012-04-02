
module MapString = Map.Make(String)
module SetString = Set.Make(String)

open OASISTypes

let () = 
  (* Compute build depends *)
  let pkg = setup_t.BaseSetup.package in 
  let findlib_of_name =
    let mp = OASISLibrary.findlib_name_map pkg in
      OASISLibrary.findlib_of_name ~recurse:true mp
  in
  let mp_int, set_ext =
    (* Collect dependencies and external dependencies from the package. *)
    List.fold_left
      (fun (mp_int, set_ext) ->
         function
           | Library (cs, bs, lib) ->
               let deps, set_ext =
                 List.fold_left
                   (fun (deps, set_ext) sct ->
                      let deps = 
                        match sct with 
                          | InternalLibrary nm ->
                              SetString.add (findlib_of_name nm) deps
                          | FindlibPackage (fndlb_pkg, _) ->
                              SetString.add fndlb_pkg deps
                      in
                      let set_ext =
                        match sct with 
                          | InternalLibrary _ ->
                              set_ext
                          | FindlibPackage (fndlb_pkg, _) ->
                              SetString.add fndlb_pkg set_ext
                      in
                        deps, set_ext)
                     (SetString.empty, set_ext)
                     bs.bs_build_depends
               in
                 MapString.add (findlib_of_name cs.cs_name) deps mp_int, 
                 set_ext
           | Executable (cs, bs, exec) ->
               let set_ext =
                 List.fold_left
                   (fun set_ext ->
                      function
                        | InternalLibrary _ ->
                            set_ext
                        | FindlibPackage (fndlb_pkg, _) ->
                            SetString.add fndlb_pkg set_ext)
                   set_ext
                   bs.bs_build_depends
               in
                 mp_int, set_ext
           | _ ->
               mp_int, set_ext)
      (MapString.empty, SetString.empty)
      pkg.sections
  in
  let mp = 
    (* Expand external dependencies. *)
    SetString.fold
      (fun fndlb_nm mp ->
         let lst =
           BaseExec.run_read_output
             "ocamlfind" 
             ["query"; fndlb_nm; "-recursive"; "-p-format"]
         in
         let set_deps = List.fold_right SetString.add lst SetString.empty in
           MapString.add fndlb_nm set_deps mp)
      set_ext mp_int
  in
  let rec transitive_closure nm visited =
    if not (SetString.mem nm visited) then
      begin
        let visited = SetString.add nm visited in
        let set =
          try  
            MapString.find nm mp
          with Not_found ->
            SetString.empty
        in
          SetString.fold transitive_closure set visited
      end
    else 
      visited
  in
  let chn = open_out "src/cli/PluginsLoaded.ml" in
    List.iter
      (function 
         | Executable (cs, bs, _) ->
             let st =
               List.fold_left
                 (fun st ->
                    function 
                      | InternalLibrary nm ->
                          transitive_closure (findlib_of_name nm) st
                      | FindlibPackage (fndlb_nm, _) ->
                          transitive_closure fndlb_nm st)
                 SetString.empty bs.bs_build_depends
             in
               Printf.fprintf chn
                 "let exec_%s_build_depends_rec = [%s]\n" 
                 (OASISUtils.varname_of_string cs.cs_name)
                 (String.concat "; "
                    (List.rev_map (Printf.sprintf "%S") (SetString.elements st)))
         | _ ->
             ())
      pkg.sections;
    close_out chn
