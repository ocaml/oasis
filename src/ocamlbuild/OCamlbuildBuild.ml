
(** Runtime support for autobuild/OCamlbuild
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
      List.rev_append rtargets (Array.to_list argv)
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
          (fun acc (nm, lib) ->
             if var_choose lib.lib_build then
               begin
                 let acc =
                   (* Compute what libraries should be built *)
                   let target ext =
                     Std (Filename.concat lib.lib_path (nm^ext))
                   in
                   let byte, native =
                     target ".cma", target ".cmxa"
                   in
                     match lib.lib_compiled_object, ocamlbest () with 
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
                   if lib.lib_c_sources <> [] then
                     CLibrary (lib.lib_path, nm) :: acc
                   else
                     acc
                 in
                   acc
               end
             else
               acc)
          []
          pkg.libraries;

        List.fold_left
          (fun acc (nm, exec) ->
             if var_choose exec.exec_build then
               begin
                 let target ext =
                   let src = 
                     (Filename.chop_extension exec.exec_main_is)^ext
                   in
                     if src = exec.exec_is then
                       Std src
                     else
                       Rename (src, exec.exec_is)
                 in
                 let byte, native = 
                   target ".byte", target ".native" 
                 in
                   match exec.exec_compiled_object, ocamlbest () with
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
               acc)
          []
          pkg.executables;
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
               ocamlbuild_run (src :: acc);
               BaseFileUtil.cp 
                 (in_build_dir src) 
                 (in_build_dir tgt);
               [])
      []
      (!cond_targets_hook cond_targets)
  in
    if last_rtargets <> [] then
      ocamlbuild_run last_rtargets

let clean pkg extra_args  = 
  (* TODO use ocamlbuild *)
  BaseExec.run "ocamlbuild" ("-clean" :: (Array.to_list extra_args))

(* END EXPORT *)

open BaseFileGenerate
open BaseUtils
open BaseMessage
open CommonGettext
open ODN
open BasePlugin

let create_ocamlbuild_files pkg () = 

  let clib path nm c_sources =  
    if c_sources <> [] then
      let fn_clib = 
        FilePath.add_extension 
          (FilePath.concat path ("lib"^nm))
          "clib"
      in
        file_generate 
          fn_clib
          comment_ocamlbuild
          (Split 
             ([], 
              List.map 
                (fun fn -> FilePath.replace_extension fn "o")
                c_sources,
              []))
  in

  let tags_of_package pkg =
    let tags_of_build_depends target deps acc = 
      List.fold_left 
        (fun acc dep ->
           match dep with 
             | FindlibPackage (findlib_pkg, _) ->
                 (target^": pkg_"^findlib_pkg) :: acc
             | InternalLibrary nm ->
                 (target^": use_"^nm) :: acc)
        acc
        deps
    in

    let target_ml dir = 
      Printf.sprintf "<%s/*.ml>" dir
    in

    let target_lib dir nm comp =
      let ext = 
        match comp with
          | Best ->
              "{cma,cmxa}"
          | Byte ->
              "cma"
          | Native ->
              "cmxa"
      in
        Printf.sprintf "<%s/%s.%s>" dir nm ext
    in

    let target_exec main_is comp =
      let ext =
        match comp with 
          | Best ->
              "{native,byte}"
          | Byte -> 
              "byte"
          | Native ->
              "native"
      in
        Printf.sprintf "<%s.%s>" (FilePath.chop_extension main_is) ext
    in

    let rev_content = 
      (* Extract content for libraries *)
      List.fold_left 
        (fun acc (nm, lib) ->

           let start_comment acc = 
             (Printf.sprintf "# Library %s" nm) :: acc
           in

           (* Add tags for build depends *)
           let pkg_tags acc = 
             let all_path = 
               List.fold_left
                 (fun st modul ->
                    SetString.add
                      (FilePath.dirname 
                         (FilePath.concat lib.lib_path modul))
                      st)
                 SetString.empty
                 lib.lib_modules
             in
               SetString.fold
                 (fun dir ->
                    tags_of_build_depends 
                      (target_ml dir) 
                      (lib.lib_build_depends @ pkg.build_depends))
                 all_path
                 acc
           in

           (* Add tag for dependency on C part of the library *)
           let clib_tag acc =
             if lib.lib_c_sources <> [] then
               (
                 (target_lib lib.lib_path nm lib.lib_compiled_object)^
                 ": use_lib"^nm
               ) :: acc
             else
               acc
           in

             clib_tag (pkg_tags (start_comment acc)))
        []
        pkg.libraries
    in

    let rev_content =
      (* Extract content for executables *)
      List.fold_left
        (fun acc (nm, exec) ->
           let dir = 
             FilePath.dirname exec.exec_main_is
           in
           let all_build_depends = 
             exec.exec_build_depends @ pkg.build_depends
           in
           let target_exec = 
             target_exec exec.exec_main_is exec.exec_compiled_object
           in

           let start_comment acc = 
             (Printf.sprintf "# Executable %s" nm) :: acc
           in
           let pkg_src_tags acc =
             tags_of_build_depends
               (target_ml dir)
               all_build_depends
               acc
           in
           let pkg_exec_tags acc = 
             tags_of_build_depends
               target_exec
               all_build_depends
               acc
           in
           let clib_tag acc = 
             if exec.exec_c_sources <> [] then
               (Printf.sprintf "%s: use_lib%s" target_exec nm) :: acc
             else
               acc
           in
           let custom_tag acc = 
             if exec.exec_custom then
               (Printf.sprintf "%s: custom" target_exec) :: acc
             else
               acc
           in
             custom_tag (clib_tag (pkg_exec_tags (pkg_src_tags (start_comment acc)))))

        rev_content
        pkg.executables
    in

      (* Filter duplicate and reverse content *)
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
           rev_content)
  in

  (* Generate files for OCaml library (.mllib/.clib) *)
  List.iter
    (fun (nm, lib) ->
       let fn_base =
         Filename.concat lib.lib_path nm
       in

       (* Generate .mllib file *)
       let () =
         let fn_mllib =
           FilePath.add_extension fn_base  "mllib"
         in
           if lib.lib_modules = [] then
             warning 
               (Printf.sprintf
                  (f_ "No module defined for library %s")
                  nm);
           file_generate
             fn_mllib
             comment_ocamlbuild
             (Split ([], lib.lib_modules, []))
       in

       (* Generate .clib file *)
       let () = 
         clib lib.lib_path nm lib.lib_c_sources
       in

         ())
    pkg.libraries;

  (* Generate files for OCaml executable (.clib) *)
  List.iter 
    (fun (nm, exec) ->
       clib (FilePath.dirname exec.exec_is) nm exec.exec_c_sources)
    pkg.executables;

  (* Generate _tags *)
  file_generate
    "_tags"
    comment_ocamlbuild
    (Split
       ([],
        tags_of_package pkg,
        []));

  (* Generate myocamlbuild.ml *)
  mlfile_generate 
    "myocamlbuild.ml"
    (let myocamlbuild_t = 
       REC
         ("OCamlbuildBase",
          [
            "lib_ocaml",
            (LST
               (List.map 
                  (fun (nm, lib) ->
                     let dirs = 
                       List.map 
                         (fun s -> STR s)
                         (SetString.elements
                            (set_string_of_list
                               (lib.lib_path
                                ::
                                List.rev_map
                                  FilePath.UnixPath.dirname
                                  (List.rev_map
                                     (FilePath.UnixPath.concat lib.lib_path)
                                     lib.lib_modules))))
                     in
                       TPL 
                         [STR (lib.lib_path^"/"^nm); 
                          LST dirs])
                  pkg.libraries));

            "lib_c",
            (LST
               (List.map 
                  (fun (nm, path) ->
                     TPL [STR nm; STR path])
                  (
                    (* Extract OCaml library/clib *)
                    (List.fold_left 
                       (fun acc (nm, lib) ->
                          if lib.lib_c_sources <> [] then
                            (nm, lib.lib_path) :: acc
                          else
                            acc)
                       []
                       pkg.libraries)
                    @
                    (* Extract OCaml executable/clib *)
                    (List.fold_left
                       (fun acc (nm, exec) ->
                          if exec.exec_c_sources <> [] then
                            (nm, FilePath.dirname exec.exec_is) :: acc
                          else
                            acc)
                       []
                       pkg.executables)
                  )))
          ])
     in
     let content = 
       [
         BaseData.basesysenvironment_ml;
         OCamlbuildData.myocamlbuild_ml;
         (
           Format.fprintf Format.str_formatter
             "@[<hv2>let package_default =@ %a@,@];;"
             (pp_odn ~opened_modules:[]) myocamlbuild_t;
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


let plugin_main pkg =
    {
      moduls       = [OCamlbuildData.ocamlbuildsys_ml];
      setup        = func build "OCamlbuildBuild.build";
      clean        = Some (func clean "OCamlbuildBuild.clean");
      distclean    = None;
      other_action = create_ocamlbuild_files pkg;
    },
    {pkg with 
         build_tools = "ocamlbuild" :: pkg.build_tools}
