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

(** Build using ocamlbuild
    @author Sylvain Le Gall
  *)

open OASISTypes
open OASISGettext
open OASISUtils
open BaseEnv
open OCamlbuildCommon
open BaseStandardVar
open BaseMessage

let cond_targets_hook =
  ref (fun lst -> lst)

let build pkg argv =

  (* Return the filename in build directory *)
  let in_build_dir fn =
    Filename.concat
      (build_dir argv)
      fn
  in

  (* Return the unix filename in host build directory *)
  let in_build_dir_of_unix fn =
    in_build_dir (BaseFilePath.of_unix fn)
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

                 let ends_with nd fn =
                   let nd_len =
                     String.length nd
                   in
                     (String.length fn >= nd_len)
                     &&
                     (String.sub
                        fn
                        (String.length fn - nd_len)
                        nd_len) = nd
                 in

                 let tgts =
                   List.flatten
                     (List.filter
                        (fun l -> l <> [])
                        (List.map
                           (List.filter
                              (fun fn ->
                                 ends_with ".cma" fn ||
                                 ends_with ".cmxa" fn ||
                                 ends_with (ext_lib ()) fn ||
                                 ends_with (ext_dll ()) fn))
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

           | Executable (cs, bs, exec) when var_choose bs.bs_build ->
               begin
                 let evs, unix_exec_is, unix_dll_opt =
                   BaseBuilt.of_executable
                     in_build_dir_of_unix
                     (cs, bs, exec)
                 in

                 let target ext =
                   let unix_tgt =
                     (BaseFilePath.Unix.concat
                        bs.bs_path
                        (BaseFilePath.Unix.chop_extension
                           exec.exec_main_is))^ext
                   in
                   let evs = 
                     (* Fix evs, we want to use the unix_tgt, without copying *)
                     List.map
                       (function
                          | BaseBuilt.BExec, nm, lst when nm = cs.cs_name ->
                              BaseBuilt.BExec, nm, [[in_build_dir_of_unix unix_tgt]]
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

           | Library _ | Executable _ | Test _
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
         if not (List.exists Sys.file_exists fns) then
           failwithf
             (f_ "No one of expected built files %s exists")
             (String.concat (s_ ", ") (List.map (Printf.sprintf "'%s'") fns)))
      lst;
      (BaseBuilt.register bt bnm lst)
  in

  let cond_targets =
    (* Run the hook *)
    !cond_targets_hook cond_targets
  in

    (* Run a list of target... *)
    run_ocamlbuild 
      (List.flatten 
         (List.map snd cond_targets))
      argv;
    (* ... and register events *)
    List.iter
      check_and_register
      (List.flatten (List.map fst cond_targets))


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
open OASISValues
open MyOCamlbuildBase
open Ocamlbuild_plugin
open OCamlbuildId

let plugin =
  `Build, name, Some version

let self_id, all_id =
  Build.create plugin

(* TODO: check everywhere that having .h and .c in CSources
 * doesn't disturb things too much
 *)
let only_h_files lst =
  List.filter
    (fun fn -> FilePath.UnixPath.check_extension fn "h")
    lst

let only_c_files lst =
  List.filter
    (fun fn -> FilePath.UnixPath.check_extension fn "c")
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
    List.rev_map FilePath.UnixPath.dirname
      (List.rev_map (prepend_bs_path bs) files)
  in
    (* Unique elements *)
    SetString.elements
      (set_string_of_list
         (List.rev_map
            (FilePath.UnixPath.reduce ~no_symlink:true)
            (bs.bs_path :: subdirs)))


let bs_tags pkg sct cs bs src_dirs src_internal_dirs link_tgt ctxt tag_t myocamlbuild_t =

  let link_pkg =
    (* Only link findlib package with executable *)
    match sct with
      | Executable _ -> true
      | Library _ | Flag _ | Test _ | SrcRepo _ | Doc _ -> false
  in

  let src_tgts =
    List.rev_append
      (* .ml files *)
      (List.rev_map
         (fun dn -> Tag.filename_concat dn "*.ml{,i}")
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
           "dll%s.so";
           "lib%s.a";
           (* Win32 *)
           "dll%s.dll";
           "lib%s.lib";
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
          FilePath.add_extension
            (prepend_bs_path bs ("lib"^cs.cs_name))
            "clib"
        in
          add_file
            (template_make
               fn_clib
               comment_ocamlbuild
               []
               (List.map
                  (fun fn -> FilePath.replace_extension fn "o")
                  (only_c_files bs.bs_c_sources))
               [])
            ctxt,

          add_tags
            tag_t
            [link_tgt]
            ["use_lib"^cs.cs_name],

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
      add_tags
        tag_t
        (if link_pkg then
           link_tgt :: src_tgts
         else
           src_tgts)
        (List.fold_left
           (fun acc ->
              function
                | FindlibPackage (findlib_pkg, _) ->
                    ("pkg_"^findlib_pkg) :: acc
                | InternalLibrary nm ->
                    ("use_"^nm) :: acc)
           []
           (OASISSection.MapSection.find sct mp))
  in

  (* Fix for PR#5015, unable to compile depends in subdir *)
  let tag_t =
    let dirs =
      src_dirs @ src_internal_dirs
    in
      (if List.length dirs > 1 then
         add_tags
           tag_t
           (List.filter
              (fun fn -> not (FilePath.UnixPath.is_current fn))
              dirs)
           ["include"]
       else
         tag_t)
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

let add_ocamlbuild_files ctxt pkg =

  let ctxt, tag_t, myocamlbuild_t =
    List.fold_left
      (fun (ctxt, tag_t, myocamlbuild_t) ->
         function
           | Library (cs, bs, lib) as sct ->
               begin
                 (* Extract content for libraries *)

                 (* All paths accessed from within the library *)
                 let src_dirs =
                   bs_paths bs lib.lib_modules
                 in

                 (* All paths accessed only by the library *)
                 let src_internal_dirs =
                   let set_dirs =
                     set_string_of_list
                       src_dirs
                   in
                   let set_internal_dirs =
                     set_string_of_list
                       (bs_paths bs lib.lib_internal_modules)
                   in
                     SetString.elements
                       (SetString.diff
                          set_internal_dirs
                          set_dirs)
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
                       (FilePath.UnixPath.add_extension
                          cs.cs_name
                          ext)
                 in

                 (* Start comment *)
                 let tag_t =
                   (Printf.sprintf "# Library %s" cs.cs_name) :: tag_t
                 in

                 (* Add include tag if the library is internal *)
                 let tag_t =
                   add_tags
                     tag_t
                     (List.filter
                        (fun fn -> not (FilePath.UnixPath.is_current fn))
                        (src_dirs @ src_internal_dirs))
                     ["include"]
                 in

                 let tag_t =
                   if lib.lib_pack then
                     let base_sources =
                       OASISLibrary.source_unix_files
                         ~ctxt:ctxt.ctxt
                         (cs, bs, lib)
                         (fun ufn ->
                            Sys.file_exists (BaseFilePath.of_unix ufn))
                     in
                     add_tags
                       tag_t
                       (List.rev_map
                          (fun (base_fn, _) ->
                             base_fn^".cmx")
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
                          (prepend_bs_path bs cs.cs_name,
                           List.filter
                             (fun fn -> not (FilePath.UnixPath.is_current fn))
                             src_dirs) :: myocamlbuild_t.lib_ocaml}
                 in

                 let () =
                   if lib.lib_modules = [] then
                     warning
                       ~ctxt:ctxt.ctxt
                       (f_ "No exported module defined for library %s")
                       cs.cs_name;
                 in

                 let ctxt =
                   (* Generate .mllib or .mlpack files *)
                   let extension, not_extension =
                     if lib.lib_pack then
                       "mlpack", "mllib"
                     else
                       "mllib", "mlpack"
                   in
                   let fn_base = prepend_bs_path bs cs.cs_name in
                   let fn = FilePath.add_extension fn_base extension in
                   let not_fn = FilePath.add_extension fn_base not_extension in
                   let ctxt =
                     add_file
                       (template_make
                          fn
                          comment_ocamlbuild
                          []
                          (lib.lib_modules @ lib.lib_internal_modules)
                          [])
                       ctxt
                   in
                     {ctxt with
                          other_actions =
                            (fun ()->
                               if Sys.file_exists not_fn then
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
                 let src_dirs =
                   bs_paths bs [exec.exec_main_is]
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
                       (FilePath.UnixPath.replace_extension
                          (exec.exec_main_is) ext)
                 in

                 let tag_t =
                   (Printf.sprintf "# Executable %s" cs.cs_name) :: tag_t
                 in

                 let ctxt, tag_t, myocamlbuild_t =
                   bs_tags
                     pkg sct cs bs
                     src_dirs
                     []
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
      (ctxt, [], {lib_ocaml = []; lib_c = []; flags = []})
      pkg.sections
  in

  (* Filter duplicate and reverse content in tag_t *)
  let tag_t =
    snd
      (List.fold_left
         (fun (prev_tag, acc) tag ->
            if (String.length tag > 0 && tag.[0] = '#') || (* Don't remove comment *)
               not (SetString.mem tag prev_tag) then (* Remove already seen tag *)
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
    }
  in

  let tag_t = 
    "# Ignore VCS directories, you can use the same kind of rule outside "
    ::
    "# OASIS_START/STOP if you want to exclude directories that contains "
    ::
    "# useless stuff for the build process"
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
            "let dispatch_default = \
                   MyOCamlbuildBase.dispatch_default package_default;;";
            "";
          ]
          ["Ocamlbuild_plugin.dispatch dispatch_default;;"];
      ]
  in

    ctxt

let qstrt_completion pkg =
  fix_build_tools (ExternalTool "ocamlbuild") pkg

let init () =
  let doit ctxt pkg =
    let ctxt =
      add_ocamlbuild_files ctxt pkg
    in

      ctxt,
      {
        chng_moduls       = [OCamlbuildData.ocamlbuildsys_ml];
        chng_main         = ODNFunc.func build "OCamlbuildPlugin.build";
        chng_clean        = Some (ODNFunc.func clean "OCamlbuildPlugin.clean");
        chng_distclean    = None;
      }
  in
    OCamlbuildId.init ();
    Build.register_act self_id doit;
    register_quickstart_completion all_id qstrt_completion
