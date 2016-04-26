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


(** Build using ocamlbuild
    @author Sylvain Le Gall
*)


open OASISTypes
open OASISGettext
open OASISUtils
open OASISString
open BaseEnv
open OCamlbuildCommon
open BaseStandardVar
open BaseMessage


    TYPE_CONV_PATH "OCamlbuildPlugin"


let cond_targets_hook =
  ref (fun lst -> lst)


let build extra_args pkg argv =
  (* Return the filename in build directory *)
  let in_build_dir fn =
    Filename.concat
      (build_dir argv)
      fn
  in

  (* Return the unix filename in host build directory *)
  let in_build_dir_of_unix fn =
    in_build_dir (OASISHostPath.of_unix fn)
  in

  let cond_targets =
    List.fold_left
      (fun acc ->
         function
           | Library (cs, bs, lib) when var_choose bs.bs_build ->
             begin
               let evs, unix_files =
                 BaseBuilt.of_library
                   in_build_dir_of_unix
                   (cs, bs, lib)
               in

               let tgts =
                 List.flatten
                   (List.filter
                      (fun l -> l <> [])
                      (List.map
                         (List.filter
                            (fun fn ->
                               ends_with ~what:".cma" fn
                               || ends_with ~what:".cmxs" fn
                               || ends_with ~what:".cmxa" fn
                               || ends_with ~what:(ext_lib ()) fn
                               || ends_with ~what:(ext_dll ()) fn))
                         unix_files))
               in

               match tgts with
                 | _ :: _ ->
                   (evs, tgts) :: acc
                 | [] ->
                   failwithf
                     (f_ "No possible ocamlbuild targets for library %s")
                     cs.cs_name
             end

           | Object (cs, bs, obj) when var_choose bs.bs_build ->
             begin
               let evs, unix_files =
                 BaseBuilt.of_object
                   in_build_dir_of_unix
                   (cs, bs, obj)
               in

               let tgts =
                 List.flatten
                   (List.filter
                      (fun l -> l <> [])
                      (List.map
                         (List.filter
                            (fun fn ->
                               ends_with ".cmo" fn
                               || ends_with ".cmx" fn))
                         unix_files))
               in

               match tgts with
                 | _ :: _ ->
                   (evs, tgts) :: acc
                 | [] ->
                   failwithf
                     (f_ "No possible ocamlbuild targets for object %s")
                     cs.cs_name
             end

           | Executable (cs, bs, exec) when var_choose bs.bs_build ->
             begin
               let evs, unix_exec_is, unix_dll_opt =
                 BaseBuilt.of_executable
                   in_build_dir_of_unix
                   (cs, bs, exec)
               in

               let target ext =
                 let unix_tgt =
                   (OASISUnixPath.concat
                      bs.bs_path
                      (OASISUnixPath.chop_extension
                         exec.exec_main_is))^ext
                 in
                 let evs =
                   (* Fix evs, we want to use the unix_tgt, without copying *)
                   List.map
                     (function
                       | BaseBuilt.BExec, nm, lst when nm = cs.cs_name ->
                         BaseBuilt.BExec, nm,
                         [[in_build_dir_of_unix unix_tgt]]
                       | ev ->
                         ev)
                     evs
                 in
                 evs, [unix_tgt]
               in

               (* Add executable *)
               let acc =
                 match bs.bs_compiled_object with
                   | Native ->
                     (target ".native") :: acc
                   | Best when bool_of_string (is_native ()) ->
                     (target ".native") :: acc
                   | Byte
                   | Best ->
                     (target ".byte") :: acc
               in
               acc
             end

           | Library _ | Object _ | Executable _ | Test _
           | SrcRepo _ | Flag _ | Doc _ ->
             acc)
      []
      (* Keep the pkg.sections ordered *)
      (List.rev pkg.sections);
  in

  (* Check and register built files *)
  let check_and_register (bt, bnm, lst) =
    List.iter
      (fun fns ->
         if not (List.exists OASISFileUtil.file_exists_case fns) then
           failwithf
             (fn_
                "Expected built file %s doesn't exist."
                "None of expected built files %s exists."
                (List.length fns))
             (String.concat (s_ " or ") (List.map (Printf.sprintf "'%s'") fns)))
      lst;
    (BaseBuilt.register bt bnm lst)
  in

  (* Run the hook *)
  let cond_targets = !cond_targets_hook cond_targets in

  (* Run a list of target... *)
  run_ocamlbuild (List.flatten (List.map snd cond_targets) @ extra_args) argv;
  (* ... and register events *)
  List.iter check_and_register (List.flatten (List.map fst cond_targets))


let clean pkg extra_args  =
  run_clean extra_args;
  List.iter
    (function
      | Library (cs, _, _) ->
        BaseBuilt.unregister BaseBuilt.BLib cs.cs_name
      | Executable (cs, _, _) ->
        BaseBuilt.unregister BaseBuilt.BExec cs.cs_name;
        BaseBuilt.unregister BaseBuilt.BExecLib cs.cs_name
      | _ ->
        ())
    pkg.sections


(* END EXPORT *)


open OASISFileTemplate
open OASISUtils
open OASISMessage
open OASISGettext
open ODN
open OASISPlugin
open OASISTypes
open OASISSchema
open MyOCamlbuildBase
open Ocamlbuild_plugin
open OCamlbuildId


let plugin =
  `Build, name, Some version


let self_id, all_id =
  Build.create plugin


let pure_interface_test =
  OASISFeatures.package_test
    (OASISFeatures.create "pure_interface" ~plugin
       OASISFeatures.alpha
       (fun () ->
          s_ "Allow to have module with only .mli file."))


let pivot_data =
  data_new_property plugin


let only_h_files lst =
  List.filter
    (fun fn -> OASISUnixPath.check_extension fn "h")
    lst


let only_c_files lst =
  List.filter
    (fun fn -> OASISUnixPath.check_extension fn "c")
    lst


let add_tags tag_t tgts tags =
  let quote_target fn =
    let test_char =
      String.contains fn
    in
    if test_char '*' ||
       test_char '?' ||
       test_char '{' ||
       test_char '[' then
      "<"^fn^">"
    else
      "\""^fn^"\""
  in

  List.fold_left
    (fun tag_t tgt ->
       List.fold_left
         (fun tag_t tag ->
            ((quote_target tgt)^": "^tag) :: tag_t)
         tag_t
         tags)
    tag_t
    tgts


let prepend_bs_path bs fn =
  Tag.filename_concat bs.bs_path fn


let bs_paths bs files =
  let subdirs =
    List.rev_map OASISUnixPath.dirname
      (List.rev_map (prepend_bs_path bs) files)
  in
  (* Unique elements *)
  SetString.elements
    (SetString.of_list
       (List.rev_map OASISUnixPath.reduce (bs.bs_path :: subdirs)))


let bs_tags pkg sct cs bs src_dirs src_internal_dirs link_tgt ctxt tag_t
    myocamlbuild_t =

  let link_pkg =
    (* Only link findlib package with executable *)
    match sct with
      | Executable _ -> true
      | Library _ | Object _ | Flag _ | Test _ | SrcRepo _ | Doc _ -> false
  in

  let src_tgts =
    List.rev_append
      (* .ml files *)
      (List.rev_map
         (fun dn -> Tag.filename_concat dn "*.ml{,i,y}")
         (src_dirs @ src_internal_dirs))
      (* .c files *)
      (List.map
         (prepend_bs_path bs)
         (only_c_files bs.bs_c_sources))
  in

  let clib_tgts =
    if bs.bs_c_sources <> [] then
      (List.rev_map
         (fun fmt -> (prepend_bs_path bs) (Printf.sprintf fmt cs.cs_name))
         [
           (* Unix *)
           "dll%s_stubs.so";
           "lib%s_stubs.a";
           (* Win32 *)
           "dll%s_stubs.dll";
           "lib%s_stubs.lib";
         ])
    else
      []
  in

  (* Manipulate ocamlbuild's spec *)
  let atom s = A s
  in
  let path s = P s
  in
  let mkspec pre_arg tr_arg lst =
    S (List.fold_left
        (fun acc arg ->
           match pre_arg with
             | Some s -> s :: arg :: acc
             | None -> arg :: acc)
        []
        (List.rev_map tr_arg lst))
  in

  (* Create flag for extra command line option *)
  let ctxt, tag_t, myocamlbuild_t =
    List.fold_left
      (fun ((ctxt, tag_t, myocamlbuild_t) as acc)
        (basename, tgts, tags, pre_arg, tr_arg, args_cond) ->
        if args_cond <> [OASISExpr.EBool true, []] then
          begin
            let tag_name =
              (* e.g. oasis_library_foo_ccopt *)
              varname_concat
                "oasis_"
                (varname_concat
                   (varname_of_string
                      (OASISSection.string_of_section sct))
                   basename)
            in
            let all_tags =
              tag_name :: tags
            in
            let cond_specs =
              List.map
                (fun (cond, lst) ->
                   cond, mkspec pre_arg tr_arg lst)
                args_cond
            in
            let flags =
              (all_tags, cond_specs) :: myocamlbuild_t.flags
            in
            let tag_t =
              add_tags tag_t tgts [tag_name]
            in
            ctxt,
            tag_t,
            {myocamlbuild_t with flags = flags}
          end
        else
          begin
            acc
          end)

      (ctxt, tag_t, myocamlbuild_t)

      ([
        "ccopt",
        src_tgts, ["compile"],
        Some (A"-ccopt"),
        atom,
        bs.bs_ccopt;

        "cclib",
        [link_tgt],
        ["link"],
        Some (A"-cclib"),
        atom,
        bs.bs_cclib;

        "cclib",
        clib_tgts,
        ["ocamlmklib"; "c"],
        None,
        atom, bs.bs_cclib;

        "dlllib",
        [link_tgt],
        ["link"; "byte"],
        Some (A"-dllib"),
        path,
        bs.bs_dlllib;

        "dllpath",
        [link_tgt],
        ["link"; "byte"],
        Some (A"-dllpath"),
        path,
        bs.bs_dllpath;

        "dllpath",
        clib_tgts,
        ["ocamlmklib"; "c"],
        Some(A"-dllpath"),
        path,
        bs.bs_dllpath;
      ]
       @
         (List.fold_left
            (fun acc flg ->
               ("byte",
                link_tgt :: src_tgts,
                ["ocaml"; flg; "byte"],
                None,
                atom,
                bs.bs_byteopt)
               ::
                 ("native",
                  link_tgt :: src_tgts,
                  ["ocaml"; flg; "native"],
                  None,
                  atom,
                  bs.bs_nativeopt)
               ::
                 acc)
            []
            ["compile";
             "ocamldep";
             "link"]))
  in

  (* Add tag for dependency on C part of the library *)
  let ctxt, tag_t, myocamlbuild_t =
    if bs.bs_c_sources <> [] then
      begin
        (* Generate .clib files *)
        let fn_clib =
          OASISHostPath.add_extension
            (prepend_bs_path bs ("lib"^(nm_libstubs cs.cs_name)))
            "clib"
        in
        add_file
          (template_make
             fn_clib
             comment_ocamlbuild
             []
             (List.map
                (fun fn -> (Filename.chop_extension fn)^".o")
                (only_c_files bs.bs_c_sources))
             [])
          ctxt,

        add_tags
          tag_t
          [link_tgt]
          [tag_libstubs cs.cs_name],

        {myocamlbuild_t with
           lib_c =
             ((cs.cs_name,
               bs.bs_path,
               List.map
                 (fun fn -> prepend_bs_path bs fn)
                 (only_h_files bs.bs_c_sources))
              ::
                myocamlbuild_t.lib_c)}
      end
    else
      ctxt, tag_t, myocamlbuild_t
  in

  (* Add build depends tags *)
  let tag_t =
    let mp =
      OASISBuildSection.transitive_build_depends pkg
    in
    let supports_ocamlfind = ocamlbuild_supports_ocamlfind pkg in
    add_tags
      tag_t
      (if link_pkg then
         link_tgt :: src_tgts
       else
         src_tgts)
      (List.sort String.compare
         (List.fold_left
            (fun acc ->
               function
                 | FindlibPackage (findlib_pkg, _) ->
                   (if supports_ocamlfind then
                      ("package("^findlib_pkg^")")
                    else
                      ("pkg_"^findlib_pkg)
                   )
                   :: acc
                 | InternalLibrary nm ->
                   ("use_"^nm) :: acc)
            []
            (OASISSection.MapSection.find sct mp)))
  in

  let ctxt =
    (* TODO: merge with qstr_cmplt *)
    set_error
      (not (List.mem (ExternalTool "ocamlbuild") bs.bs_build_tools))
      (Printf.sprintf
         (f_ "ocamlbuild in field BuildTools of %s is mandatory")
         (OASISSection.string_of_section sct))
      ctxt
  in

  ctxt, tag_t, myocamlbuild_t


module MapDirs =
  Map.Make
    (struct
      type t = [`Library of string | `Object of string | `Executable of string]
      let compare t1 t2 =
        match t1, t2 with
          | `Library _, `Executable _
          | `Library _, `Object _
          | `Object _, `Executable _ ->
            -1
          | `Object _, `Library _
          | `Executable _, `Library _
          | `Executable _, `Object _ ->
            1
          | `Library s1, `Library s2
          | `Object s1, `Object s2
          | `Executable s1, `Executable s2 ->
            String.compare s1 s2
    end)


let compute_map_dirs pkg =
  let add k dirs internal_dirs mp =
    let src_dirs, src_internal_dirs =
      try
        MapDirs.find k mp
      with Not_found ->
        SetString.empty, SetString.empty
    in
    let add_dirs st dirs = SetString.add_list st dirs in
    MapDirs.add k
      (add_dirs src_dirs dirs,
       add_dirs src_internal_dirs internal_dirs)
      mp
  in
  let map_dirs =
    List.fold_left
      (fun mp ->
         function
           | Library (cs, bs, lib) ->
             add (`Library cs.cs_name)
               (* All paths accessed from within the library *)
               (bs_paths bs lib.lib_modules)
               (* All paths accessed only by the library *)
               (bs_paths bs lib.lib_internal_modules)
               mp

           | Object (cs, bs, obj) ->
             add (`Object cs.cs_name)
               (* All paths accessed from within the (potentially packed)
                  object *)
               (bs_paths bs obj.obj_modules)
               [] (* No internal paths *)
               mp

           | Executable (cs, bs, exec) ->
             add (`Executable cs.cs_name)
               (bs_paths bs [exec.exec_main_is])
               [] (* No internal paths *)
               mp
           | Flag _ | SrcRepo _ | Test _ | Doc _ ->
             mp)
      MapDirs.empty
      pkg.sections
  in
  (* Now get rid of internal paths that are also non internal. *)
  MapDirs.map
    (fun (src_dirs, src_internal_dirs) ->
       SetString.elements src_dirs,
       SetString.elements (SetString.diff src_internal_dirs src_dirs))
    map_dirs


let compute_includes map_dirs pkg =
  let add_includes dir set_dirs includes =
    (* Not self-dependent *)
    let set_dirs = SetString.diff set_dirs (SetString.singleton dir) in
    let pre_dirs =
      try
        MapString.find dir includes
      with Not_found ->
        SetString.empty
    in
    MapString.add dir (SetString.union set_dirs pre_dirs) includes
  in

  let add_map_dirs k bs includes =
    let dep_dirs =
      (* Source dirs of dependent libraries *)
      List.fold_left
        (fun set ->
           function
             | InternalLibrary nm ->
               let src_dirs, _ =
                 try MapDirs.find (`Library nm) map_dirs
                 with Not_found -> MapDirs.find (`Object nm) map_dirs
               in
               SetString.add_list set src_dirs

             | FindlibPackage _ ->
               set)
        SetString.empty
        bs.bs_build_depends
    in
    let self_dirs =
      (* Source dirs *)
      let src_dirs, src_internal_dirs = MapDirs.find k map_dirs in
      SetString.add_list (SetString.of_list src_dirs) src_internal_dirs
    in
    let all_dirs = SetString.union dep_dirs self_dirs in
    let all_dirs =
      (* No need to include the current dir. *)
      SetString.filter
        (fun dn -> not (OASISUnixPath.is_current_dir dn))
        all_dirs
    in

    (* All self_dirs depends on all_dirs *)
    SetString.fold
      (fun dir includes ->
         add_includes dir all_dirs includes)
      self_dirs
      includes
  in

  let includes =
    List.fold_left
      (fun includes ->
         function
           | Library (cs, bs, _) ->
             add_map_dirs (`Library cs.cs_name) bs includes
           | Object (cs, bs, _) ->
             add_map_dirs (`Object cs.cs_name) bs includes
           | Executable (cs, bs, _) ->
             add_map_dirs (`Executable cs.cs_name) bs includes
           | Flag _ | SrcRepo _ | Test _ | Doc _ ->
             includes)
      MapString.empty
      pkg.sections
  in
  MapString.fold
    (fun dir include_dirs acc ->
       if SetString.empty <> include_dirs then
         (dir, SetString.elements include_dirs) :: acc
       else
         acc)
    includes
    []


(* Check if the given files list only contains .mli. *)
let is_pure_interface (base_fn, fn_lst) =
  let rec is_pure_interface_aux =
    (* TODO: this needs to be refine because sometime we don't have the .ml file
     * because it is generated (BaseData.ml) but we have the .mli.
    *)
    function
      | [fn] ->
        OASISString.ends_with ~what:".mli" fn
      | fn :: tl ->
        OASISString.ends_with ~what:".mli" fn && is_pure_interface_aux tl
      | [] -> false
  in
  is_pure_interface_aux fn_lst


let add_ocamlbuild_files ctxt pkg =

  let map_dirs =
    compute_map_dirs pkg
  in

  let ctxt, tag_t, myocamlbuild_t =
    List.fold_left
      (fun (ctxt, tag_t, myocamlbuild_t) ->
         function
           | Library (cs, bs, lib) as sct ->
             begin
               (* Extract content for libraries *)

               (* All paths of the library *)
               let src_dirs, src_internal_dirs =
                 MapDirs.find (`Library cs.cs_name) map_dirs
               in

               (* Generated library *)
               let target_lib =
                 let ext =
                   match bs.bs_compiled_object with
                     | Best ->
                       "{cma,cmxa}"
                     | Byte ->
                       "cma"
                     | Native ->
                       "cmxa"
                 in
                 prepend_bs_path bs
                   (OASISUnixPath.add_extension cs.cs_name ext)
               in

               (* Start comment *)
               let tag_t =
                 (Printf.sprintf "# Library %s" cs.cs_name) :: tag_t
               in

               (* Sources of the library. *)
               let sources =
                 OASISLibrary.source_unix_files
                   ~ctxt:ctxt.ctxt
                   (cs, bs, lib)
                   (fun ufn ->
                      OASISFileUtil.file_exists_case
                        (OASISHostPath.of_unix ufn))
               in

               let intf_module_list, impl_module_list =
                 let to_module (base_fn, _) =
                   String.capitalize (Filename.basename base_fn)
                 in
                 let intf_module_list =
                   if pure_interface_test pkg then
                     List.map to_module
                       (List.filter is_pure_interface sources)
                   else
                     []
                 in
                 List.partition
                   (fun modul -> List.mem modul intf_module_list)
                   (lib.lib_modules @ lib.lib_internal_modules)
               in

               (* Add dependency of cmxs to their own library: used
                  at link time when there is C code *)
               let tag_t =
                 add_tags
                   tag_t
                   [prepend_bs_path
                      bs
                      (OASISUnixPath.add_extension cs.cs_name "cmxs")]
                   ["use_"^cs.cs_name]
               in

               let tag_t =
                 if lib.lib_pack then
                   add_tags
                     tag_t
                     (List.rev_map
                        (fun (base_fn, _) ->
                           OASISUnixPath.add_extension base_fn "cmx")
                        sources)
                     ["for-pack("^String.capitalize cs.cs_name^")"]
                 else
                   tag_t
               in

               let ctxt, tag_t, myocamlbuild_t =
                 bs_tags
                   pkg sct cs bs
                   src_dirs
                   src_internal_dirs
                   target_lib
                   ctxt
                   tag_t
                   myocamlbuild_t
               in

               let myocamlbuild_t =
                 {myocamlbuild_t with
                    lib_ocaml =
                      (cs.cs_name,
                       List.filter
                         (fun fn -> not (OASISUnixPath.is_current fn))
                         src_dirs,
                       intf_module_list)
                      :: myocamlbuild_t.lib_ocaml}
               in

               let () =
                 if lib.lib_modules = [] then
                   warning
                     ~ctxt:ctxt.ctxt
                     (f_ "No exported module defined for library %s")
                     cs.cs_name;
               in

               let ctxt =
                 (* Generate .mllib *)
                 let fn_base = prepend_bs_path bs cs.cs_name in
                 let mllib = OASISHostPath.add_extension fn_base "mllib" in
                 let mldylib =
                   OASISHostPath.add_extension fn_base "mldylib"
                 in
                 let mlpack = OASISHostPath.add_extension fn_base "mlpack" in
                 let mllib_template_lines =
                   (* mllib contains either the name of the pack or the list
                    * of modules. *)
                   if lib.lib_pack then
                     [ String.capitalize cs.cs_name ]
                   else
                     impl_module_list
                 in
                 let ctxt =
                   add_file
                     (template_make mllib
                        comment_ocamlbuild
                        []
                        mllib_template_lines
                        [])
                     ctxt
                 in
                 let ctxt =
                   add_file
                     (template_make mldylib
                        comment_ocamlbuild
                        []
                        mllib_template_lines
                        [])
                     ctxt
                 in
                 if lib.lib_pack then begin
                   (* generate .mlpack for packed libraries *)
                   add_file
                     (template_make mlpack
                        comment_ocamlbuild
                        []
                        impl_module_list
                        [])
                     ctxt
                 end else begin
                   (* make sure there is no conflicting mlpack file *)
                   {ctxt with
                      other_actions =
                        (fun ()->
                           if OASISFileUtil.file_exists_case mlpack then
                             OASISMessage.error ~ctxt:ctxt.ctxt
                               (f_ "Conflicting file '%s' and '%s' \
                                    exists, remove '%s'.")
                               mllib mlpack mlpack)
                        :: ctxt.other_actions}
                 end
               in
               ctxt, tag_t, myocamlbuild_t
             end

           | Object (cs, bs, obj) as sct ->
             begin
               (* Extract content for objects *)

               (* All paths of the library *)
               let src_dirs, src_internal_dirs =
                 MapDirs.find (`Object cs.cs_name) map_dirs
               in

               (* Generated library *)
               let target_lib =
                 let ext =
                   match bs.bs_compiled_object with
                     | Best ->
                       "{cmo,cmx}"
                     | Byte ->
                       "cmo"
                     | Native ->
                       "cmx"
                 in
                 prepend_bs_path bs
                   (OASISUnixPath.add_extension cs.cs_name ext)
               in

               (* Start comment *)
               let tag_t =
                 (Printf.sprintf "# Object %s" cs.cs_name) :: tag_t
               in

               let tag_t =
                 if List.length obj.obj_modules <> 1 then
                   let base_sources =
                     OASISObject.source_unix_files
                       ~ctxt:ctxt.ctxt
                       (cs, bs, obj)
                       (fun ufn ->
                          OASISFileUtil.file_exists_case
                            (OASISHostPath.of_unix ufn))
                   in
                   add_tags
                     tag_t
                     (List.rev_map
                        (fun (base_fn, _) ->
                           OASISUnixPath.add_extension base_fn "cmx")
                        base_sources)
                     ["for-pack("^String.capitalize cs.cs_name^")"]
                 else
                   tag_t
               in

               let ctxt, tag_t, myocamlbuild_t =
                 bs_tags
                   pkg sct cs bs
                   src_dirs
                   src_internal_dirs
                   target_lib
                   ctxt
                   tag_t
                   myocamlbuild_t
               in

               let myocamlbuild_t =
                 {myocamlbuild_t with
                    lib_ocaml =
                      (cs.cs_name,
                       List.filter
                         (fun fn -> not (OASISUnixPath.is_current fn))
                         src_dirs,
                       []) :: myocamlbuild_t.lib_ocaml}
               in

               let () =
                 if obj.obj_modules = [] then
                   warning
                     ~ctxt:ctxt.ctxt
                     (f_ "No exported module defined for object %s")
                     cs.cs_name;
               in

               let ctxt =
                 match obj.obj_modules with
                   | [ m ] -> ctxt
                   | _ -> (* generate mlpack file *)
                     let fn_base = prepend_bs_path bs cs.cs_name in
                     let fn =
                       OASISHostPath.add_extension fn_base "mlpack"
                     and not_fn =
                       OASISHostPath.add_extension fn_base "ml"
                     in
                     let ctxt =
                       add_file
                         (template_make
                            fn
                            comment_ocamlbuild
                            []
                            obj.obj_modules
                            [])
                         ctxt
                     in
                     {ctxt with
                        other_actions =
                          (fun () ->
                             if OASISFileUtil.file_exists_case not_fn
                             then
                               OASISMessage.error ~ctxt:ctxt.ctxt
                                 (f_ "Conflicting file '%s' and '%s' \
                                      exists, remove '%s'.")
                                 fn not_fn not_fn)
                          :: ctxt.other_actions}
               in
               ctxt, tag_t, myocamlbuild_t
             end

           | Executable (cs, bs, exec) as sct ->
             begin
               (* Extract content for executables *)
               let src_dirs, src_internal_dirs =
                 MapDirs.find (`Executable cs.cs_name) map_dirs
               in

               let target_exec =
                 let ext =
                   match bs.bs_compiled_object with
                     | Best ->
                       "{native,byte}"
                     | Byte ->
                       "byte"
                     | Native ->
                       "native"
                 in
                 prepend_bs_path bs
                   (OASISUnixPath.replace_extension
                      (exec.exec_main_is) ext)
               in

               let tag_t =
                 (Printf.sprintf "# Executable %s" cs.cs_name) :: tag_t
               in

               let ctxt, tag_t, myocamlbuild_t =
                 bs_tags
                   pkg sct cs bs
                   src_dirs
                   src_internal_dirs
                   target_exec
                   ctxt
                   tag_t
                   myocamlbuild_t
               in

               let tag_t =
                 if exec.exec_custom then
                   add_tags tag_t [target_exec] ["custom"]
                 else
                   tag_t
               in

               ctxt, tag_t, myocamlbuild_t
             end

           | Flag _ | SrcRepo _ | Test _ | Doc _ ->
             ctxt, tag_t, myocamlbuild_t)
      (ctxt, [], {lib_ocaml = []; lib_c = []; flags = []; includes = []})
      pkg.sections
  in

  (* Filter duplicate and reverse content in tag_t. *)
  let tag_t =
    snd
      (List.fold_left
         (fun (prev_tag, acc) tag ->
            (* Don't remove comment but remove already seen tag. *)
            if (String.length tag > 0 && tag.[0] = '#') ||
               not (SetString.mem tag prev_tag) then
              (
                SetString.add tag prev_tag,
                tag :: acc
              )
            else
              (prev_tag, acc))
         (SetString.empty, [])
         tag_t)
  in

  let myocamlbuild_t =
    (* Fix section order *)
    {
      lib_ocaml = List.rev myocamlbuild_t.lib_ocaml;
      lib_c     = List.rev myocamlbuild_t.lib_c;
      flags     = List.rev myocamlbuild_t.flags;
      includes  = compute_includes map_dirs pkg;
    }
  in

  let tag_t =
    "# Ignore VCS directories, you can use the same kind of rule outside"
    ::
      "# OASIS_START/STOP if you want to exclude directories that contains"
    ::
      "# useless stuff for the build process"
    ::
      "true: annot, bin_annot"
    ::
      add_tags
        tag_t
        ["_darcs"; ".git"; ".hg"; ".bzr"; "**/.svn"]
        ["not_hygienic"; "-traverse"]
  in


  let ctxt =
    List.fold_left
      (fun ctxt tmpl -> add_file tmpl ctxt)
      ctxt
      [
        (* Generate _tags *)
        template_make
          "_tags"
          comment_ocamlbuild
          []
          tag_t
          [];

        (* Generate myocamlbuild.ml *)
        template_of_mlfile
          "myocamlbuild.ml"
          []
          [
            OASISData.oasissyslight_ml;
            BaseData.basesysenvironment_ml;
            OCamlbuildData.myocamlbuild_ml;
            "open Ocamlbuild_plugin;;";
            (
              Format.fprintf Format.str_formatter
                "@[<hv2>let package_default =@ %a@,@];;"
                (pp_odn ~opened_modules:["Ocamlbuild_plugin"])
                (MyOCamlbuildBase.odn_of_t myocamlbuild_t);
              Format.flush_str_formatter ()
            );
            "";
            Printf.sprintf
              "let conf = {MyOCamlbuildFindlib.no_automatic_syntax = %b}"
              (OASISFeatures.package_test OASISFeatures.no_automatic_syntax pkg);
            "";
            "let dispatch_default = \
             MyOCamlbuildBase.dispatch_default conf package_default;;";
            "";
          ]
          ["Ocamlbuild_plugin.dispatch dispatch_default;;"];
      ]
  in

  ctxt


let generator =
  ocamlbuild_common_generator pivot_data OASISPackage.schema all_id

let doit ctxt pkg =
  let extra_args =
    extra_args_ocamlbuild_common ~ctxt:ctxt.ctxt pkg
      (generator pkg.schema_data)
  in
  let ctxt = add_ocamlbuild_files ctxt pkg in
  ctxt,
  {
    chng_moduls       = [OCamlbuildData.ocamlbuildsys_ml];
    chng_main         = ODNFunc.func_with_arg build
        "OCamlbuildPlugin.build"
        extra_args odn_of_extra_args;
    chng_clean        = Some (ODNFunc.func clean "OCamlbuildPlugin.clean");
    chng_distclean    = None;
  }


let qstrt_completion pkg =
  fix_build_tools (ExternalTool "ocamlbuild") pkg


let init () =
  OCamlbuildId.init ();
  Build.register_act self_id doit;
  register_quickstart_completion all_id qstrt_completion;
  register_generator_package all_id pivot_data generator
