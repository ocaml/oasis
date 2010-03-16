
(** Build using ocamlbuild  
    @author Sylvain Le Gall
  *)

open OASISTypes
open BaseEnv
open BaseStandardVar

type target =
  | Std of string
  | CLibrary  of string * string
  | Rename of string * string

let cond_targets_hook =
  ref (fun lst -> lst)

let build pkg argv =
  (* Fix special arguments depending on environment *)
  let env_args =
    List.flatten
      [
        if (os_type ()) = "Win32" then
          [
            "-classic-display"; 
            "-no-log"; 
            "-install-lib-dir"; 
            (Filename.concat (standard_library ()) "ocamlbuild")
          ] 
        else
          [];
    
        if (ocamlbest ()) = "byte" || (os_type ()) = "Win32" then
          [
            "-byte-plugin" 
          ]
        else
          [];
      ]
  in

  let ocamlbuild_run rtargets = 
    let args = 
      rtargets @ (Array.to_list argv)
    in
      BaseExec.run (ocamlbuild ()) (env_args @ args)
  in

  let in_build_dir fn =
    Filename.concat "_build" fn
  in

  let cond_targets =
    List.flatten 
      [
        List.fold_left
          (fun acc ->
             function 
               | Library (cs, bs, lib) ->
                   if var_choose bs.bs_build then
                     begin
                       let acc =
                         (* Compute what libraries should be built *)
                         let target ext =
                           Std (Filename.concat bs.bs_path (cs.cs_name^ext))
                         in
                         let byte, native =
                           target ".cma", target ".cmxa"
                         in
                           match bs.bs_compiled_object, ocamlbest () with 
                             | Native, _ 
                             | Best, "native" ->
                                 byte :: native :: acc
                             | Byte, _
                             | Best, "byte" ->
                                 byte :: acc
                             | Best, ocamlbest ->
                                 failwith 
                                   (Printf.sprintf 
                                      "Unknown ocamlbest: '%s'"
                                      ocamlbest)
                       in

                       let acc = 
                         (* Add C library to be built *)
                         if bs.bs_c_sources <> [] then
                           CLibrary (bs.bs_path, cs.cs_name) :: acc
                         else
                           acc
                       in
                         acc
                     end
                   else
                     acc
               | Executable (cs, bs, exec) ->
                   if var_choose bs.bs_build then
                     begin
                       let target ext =
                         let src = 
                           (Filename.concat
                              bs.bs_path
                              (Filename.chop_extension 
                                 exec.exec_main_is))^ext
                         in
                         let exec_is =
                           OASISExecutable.exec_is (cs, bs, exec)
                         in
                           if src = exec_is then
                             Std src
                           else
                             Rename (src, exec_is)
                       in
                       let byte, native = 
                         target ".byte", target ".native" 
                       in
                         match bs.bs_compiled_object, ocamlbest () with
                           | Byte, _
                           | Best, "byte" ->
                               byte :: acc
                           | Native, _
                           | Best, "native" ->
                               native :: acc
                           | Best, ocamlbest ->
                               failwith 
                                 (Printf.sprintf 
                                    "Unknown ocamlbest: '%s'"
                                    ocamlbest)
                     end
                   else
                     acc
               | Test _ | SrcRepo _ | Flag _ ->
                   acc)
          []
          (* Keep the pkg.sections ordered *)
          (List.rev pkg.sections);
      ]
  in

  let last_rtargets =
    List.fold_left
      (fun acc tgt ->
         match tgt with 
           | Std nm -> 
               nm :: acc
           | CLibrary (dir, nm) ->
               (dir^"/lib"^nm^(ext_lib ())) 
               ::
               (dir^"/dll"^nm^(ext_dll ()))
               ::
               acc
           | Rename (src, tgt) ->
               ocamlbuild_run (List.rev (src :: acc));
               BaseFileUtil.cp 
                 (in_build_dir src) 
                 (in_build_dir tgt);
               [])
      []
      (!cond_targets_hook cond_targets)
  in
    if last_rtargets <> [] then
      ocamlbuild_run (List.rev last_rtargets)

let clean pkg extra_args  = 
  BaseExec.run (ocamlbuild ()) ("-clean" :: (Array.to_list extra_args))

(* END EXPORT *)

open BaseFileGenerate
open OASISUtils
open BaseMessage
open OASISGettext
open ODN
open OASISPlugin
open OASISTypes
open OASISValues
open OCamlbuildBase
open Ocamlbuild_plugin

module PU = OASISPlugin.Build.Make
              (struct 
                 let name    = "OCamlbuild"
                 let version = OASISConf.version
               end)

let extern = 
  PU.new_field
    OASISLibrary.schema
    "Extern"
    ~default:true
    boolean
    (fun () ->
       s_ "By default we consider library to be external. This allow to \
           have very limited export. If set to false, the _tags file \
           will include all directories of the libraries. This can be a \
           problem if there are conflicting modules.")

let create_ocamlbuild_files pkg () = 

  let add_tags tag_t tgts tags = 
    List.fold_left
      (fun tag_t tgt ->
         List.fold_left
           (fun tag_t tag ->
              (tgt^": "^tag) :: tag_t)
           tag_t
           tags)
      tag_t
      tgts
  in

  let bs_paths bs files = 
    SetString.elements
     (set_string_of_list
        (bs.bs_path
         ::
         List.rev_map
           FilePath.UnixPath.dirname
           (List.rev_map
              (FilePath.UnixPath.concat bs.bs_path)
              files)))
  in

  let bs_tags sct cs bs src_dirs link_tgt tag_t myocamlbuild_t = 

    let link_pkg = 
      (* Only link findlib package with executable *)
      match sct with 
        | Executable _ -> true
        | Library _ | Flag _ | Test _ | SrcRepo _ -> false
    in

    let src_tgts = 
      List.rev_append
        (* .ml files *)
        (List.rev_map 
           (Printf.sprintf "<%s/*.ml>")
           src_dirs)
        (* .c files *)
        (List.rev_map
           (Printf.sprintf "\"%s\"")
           (List.rev_map 
              (FilePath.UnixPath.concat bs.bs_path) 
              bs.bs_c_sources))
    in

    let tag_t, myocamlbuild_t = 
      let atom s = A s
      in
      let path s = P s
      in
        List.fold_left
          (fun (tag_t, myocamlbuild_t) 
                 (basename, tags, pre_arg, tr_arg, args_cond) ->
             let args = 
               var_choose args_cond
             in
               if args <> [] then
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
                   let spec = 
                     S (List.fold_left
                          (fun acc arg -> 
                             match pre_arg with 
                               | Some s -> s :: arg :: acc
                               | None -> arg :: acc)
                          []
                          (List.rev_map tr_arg args))
                   in
                   let tag_t =
                     if List.mem "compile" tags then
                       add_tags tag_t src_tgts [tag_name]
                     else
                       tag_t
                   in
                   let tag_t =
                     if List.mem "link" tags then 
                       add_tags tag_t [link_tgt] [tag_name]
                     else
                       tag_t
                   in
                     tag_t,
                     {myocamlbuild_t with
                          flags = (all_tags, spec) :: myocamlbuild_t.flags}
                 end
               else
                 begin
                   tag_t, myocamlbuild_t
                 end)

          (tag_t, myocamlbuild_t)

          [
            "ccopt",   ["compile"], Some (A"-ccopt"),   atom, bs.bs_ccopt;
            "cclib",   ["link"],    Some (A"-cclib"),   atom, bs.bs_cclib;
            "dlllib",  ["link"],    Some (A"-dllib"),   path, bs.bs_dlllib;
            "dllpath", ["link"],    Some (A"-dllpath"), path, bs.bs_dllpath;
            "byte",    ["byte"],    None,               atom, bs.bs_byteopt;
            "native",  ["native"],  None,               atom, bs.bs_nativeopt;
          ]
    in

    (* Add tag for dependency on C part of the library *)
    let tag_t, myocamlbuild_t =
      if bs.bs_c_sources <> [] then
        begin
          (* Generate .clib files *)
          let fn_clib = 
            FilePath.add_extension 
              (FilePath.concat bs.bs_path ("lib"^cs.cs_name))
              "clib"
          in
            file_generate 
              fn_clib
              comment_ocamlbuild
              (Split 
                 ([], 
                  List.map 
                    (fun fn -> FilePath.replace_extension fn "o")
                    bs.bs_c_sources,
                  []));

            add_tags tag_t [link_tgt] ["use_lib"^cs.cs_name],
            {myocamlbuild_t with 
                 lib_c = 
                   ((cs.cs_name, bs.bs_path) 
                   :: 
                    myocamlbuild_t.lib_c)}
        end
      else
        begin
          tag_t, myocamlbuild_t
        end
    in

    (* Add build depends tags *)
    let tag_t, myocamlbuild_t =
      let mp =
        OASISBuildSection.transitive_build_depends pkg
      in
        add_tags 
          tag_t
          (if link_pkg then
             link_tgt :: src_tgts
           else
             src_tgts)
          (List.rev_map 
             (function
                | FindlibPackage (findlib_pkg, _) ->
                    "pkg_"^findlib_pkg
                | InternalLibrary nm ->
                    "use_"^nm)
             (OASISSection.MapSection.find sct mp)),
        myocamlbuild_t
    in

      tag_t, myocamlbuild_t
  in

  let tag_t, myocamlbuild_t = 
    List.fold_left 
      (fun (tag_t, myocamlbuild_t) ->
         function 
           | Library (cs, bs, lib) as sct ->
               begin
                 (* Extract content for libraries *)

                 (* All path access from within the library *)
                 let src_dirs = 
                   bs_paths bs lib.lib_modules
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
                     Printf.sprintf "<%s/%s.%s>" bs.bs_path cs.cs_name ext
                 in

                 (* Start comment *)
                 let tag_t = 
                   (Printf.sprintf "# Library %s" cs.cs_name) :: tag_t
                 in

                 (* Add include tag if the library is internal *)
                 let tag_t =
                   if not (extern cs.cs_data) then
                     add_tags 
                       tag_t
                       (List.rev_map (Printf.sprintf "\"%s\"") src_dirs)
                       ["include"]
                   else
                     tag_t
                 in

                 let tag_t, myocamlbuild_t =
                   bs_tags 
                     sct cs bs 
                     src_dirs
                     target_lib
                     tag_t
                     myocamlbuild_t
                 in

                 let myocamlbuild_t = 
                   {myocamlbuild_t with 
                        lib_ocaml = 
                          (FilePath.UnixPath.concat bs.bs_path cs.cs_name, 
                           src_dirs, 
                           extern cs.cs_data) :: myocamlbuild_t.lib_ocaml}
                 in

                 (* Generate .mllib file *)
                 let () =
                   let fn_mllib =
                     FilePath.add_extension 
                       (Filename.concat bs.bs_path cs.cs_name)
                       "mllib"
                   in
                     if lib.lib_modules = [] then
                       warning 
                         (Printf.sprintf
                            (f_ "No module defined for library %s")
                            cs.cs_name);
                     file_generate
                       fn_mllib
                       comment_ocamlbuild
                       (Split ([], lib.lib_modules, []))
                 in

                   tag_t, myocamlbuild_t
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
                     Printf.sprintf 
                       "<%s.%s>" 
                       (FilePath.UnixPath.concat 
                          bs.bs_path 
                          (FilePath.UnixPath.chop_extension 
                             (exec.exec_main_is)))
                       ext
                 in

                 let tag_t = 
                   (Printf.sprintf "# Executable %s" cs.cs_name) :: tag_t
                 in

                 let tag_t, myocamlbuild_t =
                   bs_tags 
                     sct cs bs 
                     src_dirs
                     target_exec
                     tag_t
                     myocamlbuild_t
                 in

                 let tag_t = 
                   if exec.exec_custom then
                     add_tags tag_t [target_exec] ["custom"]
                   else
                     tag_t
                 in

                   tag_t, myocamlbuild_t
               end

           | Flag _ | SrcRepo _ | Test _ ->
               tag_t, myocamlbuild_t)
      ([], {lib_ocaml = []; lib_c = []; flags = []})
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

  (* Generate _tags *)
  file_generate "_tags" comment_ocamlbuild (Split ([], tag_t, []));

  (* Generate myocamlbuild.ml *)
  mlfile_generate 
    "myocamlbuild.ml"
    (let content = 
       [
         BaseData.basesysenvironment_ml;
         OCamlbuildData.myocamlbuild_ml;
         "open Ocamlbuild_plugin;;";
         (
           Format.fprintf Format.str_formatter
             "@[<hv2>let package_default =@ %a@,@];;"
             (pp_odn ~opened_modules:["Ocamlbuild_plugin"]) 
             (OCamlbuildBase.odn_of_t myocamlbuild_t);
           Format.flush_str_formatter ()
         );
         "";
         "let dispatch_default = \
                OCamlbuildBase.dispatch_default package_default;;"; 
         "";
       ]
     in
       Split ([], 
              content, 
              ["Ocamlbuild_plugin.dispatch dispatch_default;;"]))


let () =
  let doit pkg =
      {
        moduls       = [OCamlbuildData.ocamlbuildsys_ml];
        setup        = ODNFunc.func build "OCamlbuildPlugin.build";
        clean        = Some (ODNFunc.func clean "OCamlbuildPlugin.clean");
        distclean    = None;
        other_action = create_ocamlbuild_files pkg;
      },
      OASISPackage.add_build_tool ~no_test:true 
        (ExternalTool "ocamlbuild") 
        pkg
  in
    PU.register doit
