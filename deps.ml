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


module MapString = Map.Make(String)
module SetString = Set.Make(String)


open OASISTypes
open BaseEnv


let generated_fn = OASISHostPath.of_unix "src/cli/CLIPluginsLoaded.ml"


let post_configure pkg =
  (* Compute build depends *)
  let _, findlib_of_name, _ =
    OASISFindlib.findlib_mapping pkg
  in
  let mp_int, set_ext =
    (* Collect dependencies and external dependencies from the package. *)
    List.fold_left
      (fun (mp_int, set_ext) ->
         function
           | Library (cs, bs, lib) when var_choose bs.bs_build ->
               begin
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
               end
           | Executable (cs, bs, exec) when var_choose bs.bs_build ->
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
           OASISExec.run_read_output ~ctxt:!BaseContext.default
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
  let chn = open_out generated_fn in
  let fmt = Format.formatter_of_out_channel chn in
    Format.fprintf fmt "@[<v>";
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
             let first = ref true in
               Format.fprintf fmt
                 "let exec_%s_build_depends_rec = [@[<hv>"
                 (OASISUtils.varname_of_string cs.cs_name);
               List.iter
                 (fun str ->
                    if !first then begin
                      Format.fprintf fmt "%S" str;
                      first := false
                    end else begin
                      Format.fprintf fmt ";@ %S" str
                    end)
                 (List.rev (SetString.elements st));
               Format.fprintf fmt "@]]@,"
         | _ ->
             ())
      pkg.sections;
    Format.fprintf fmt "@]@?";
    close_out chn


let setup_t =
  {setup_t with
       BaseSetup.configure =
         (fun pkg args ->
            setup_t.BaseSetup.configure pkg args;
            post_configure pkg);
       BaseSetup.distclean =
         (fun pkg args -> Sys.remove generated_fn)
         :: setup_t.BaseSetup.distclean}


let setup () =  BaseSetup.setup setup_t
