
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
  (* TODO use ocamlbuild *)
  BaseExec.run "ocamlbuild" ("-clean" :: (Array.to_list extra_args))

(* END EXPORT *)

open BaseFileGenerate
open OASISUtils
open BaseMessage
open CommonGettext
open ODN
open BasePlugin
open OASISTypes
open OASISValues
open OCamlbuildBase

let plugin_id = "OCamlbuild"

let extern = 
  OASIS.new_field
    OASISLibrary.schema
    plugin_id
    "Extern"
    ~default:true
    boolean
    (fun () ->
       s_ "By default we consider library to be external. This allow to \
           have very limited export. If set to false, the _tags file \
           will include all directories of the libraries. This can be a \
           problem if there are conflicting modules.")

let create_ocamlbuild_files pkg () = 

  let clib cs bs =  
    if bs.bs_c_sources <> [] then
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
              []))
  in

  let tags_of_package pkg =

    let build_depends_of_section = 
      let mp =
        OASISBuildSection.transitive_build_depends pkg
      in
        fun sct ->
          OASISSection.MapSection.find sct mp
    in

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

    let rev_content = 
      List.fold_left 
        (fun acc ->
           function 
             | Library (cs, bs, lib) as sct ->
                 begin
                   let build_depends =
                     build_depends_of_section sct
                   in

                   (* Extract content for libraries *)
                   let start_comment acc = 
                     (Printf.sprintf "# Library %s" cs.cs_name) :: acc
                   in

                   (* All path access from within the library *)
                   let all_paths = 
                     List.fold_left
                       (fun st modul ->
                          SetString.add
                            (FilePath.dirname 
                               (FilePath.concat bs.bs_path modul))
                            st)
                       SetString.empty
                       lib.lib_modules
                   in

                   (* Add tags for build depends *)
                   let pkg_tags acc = 
                       SetString.fold
                         (fun dir ->
                            tags_of_build_depends 
                              (target_ml dir) 
                              build_depends)
                         all_paths
                         acc
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

                   (* Add tag for dependency on C part of the library *)
                   let clib_tag acc =
                     if bs.bs_c_sources <> [] then
                       (Printf.sprintf "%s: use_lib%s" target_lib cs.cs_name) :: acc
                     else
                       acc
                   in

                  (* Add include tag if the library is internal *)
                   let include_tag acc =
                     if not (extern cs.cs_data) then
                       SetString.fold
                         (fun dir acc ->
                            (Printf.sprintf "\"%s\": include" dir)
                            ::
                            acc)
                         all_paths
                         acc
                     else
                       acc
                   in

                     include_tag (clib_tag (pkg_tags (start_comment acc)))

                 end

             | Executable (cs, bs, exec) as sct ->
                 begin
                   (* Extract content for executables *)
                   let dir = 
                     OASISExecutable.exec_main_path (cs, bs, exec)
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
                         (* TODO: here Filename.concat is Unix.Filename.Concat *)
                         (Filename.concat 
                            bs.bs_path 
                            (FilePath.chop_extension 
                               (exec.exec_main_is)))
                         ext
                   in

                   let start_comment acc = 
                     (Printf.sprintf "# Executable %s" cs.cs_name) :: acc
                   in
                   let build_depends = 
                     build_depends_of_section
                       sct 
                   in
                   let pkg_src_tags acc =
                     tags_of_build_depends
                       (target_ml dir)
                       build_depends
                       acc
                   in
                   let pkg_exec_tags acc = 
                     tags_of_build_depends
                       target_exec
                       build_depends
                       acc
                   in
                   let clib_tag acc = 
                     if bs.bs_c_sources <> [] then
                       (Printf.sprintf "%s: use_lib%s" target_exec cs.cs_name) :: acc
                     else
                       acc
                   in
                   let custom_tag acc = 
                     if exec.exec_custom then
                       (Printf.sprintf "%s: custom" target_exec) :: acc
                     else
                       acc
                   in
                     custom_tag (clib_tag (pkg_exec_tags (pkg_src_tags (start_comment acc))))
                 end

             | Flag _ | SrcRepo _ | Test _ ->
                 acc)
        []
        pkg.sections
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

  List.iter
    (function
       | Library (cs, bs, lib) ->
           begin
             (* Generate files for OCaml library (.mllib/.clib) *)
             let fn_base =
               Filename.concat bs.bs_path cs.cs_name
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
                        cs.cs_name);
                 file_generate
                   fn_mllib
                   comment_ocamlbuild
                   (Split ([], lib.lib_modules, []))
             in

               (* Generate .clib file *)
               clib cs bs
           end

       | Executable (cs, bs, exec) ->
           begin
             (* Generate files for OCaml executable (.clib) *)
             clib cs bs
           end

       | Flag _ | SrcRepo _ | Test _ ->
           ())
    pkg.sections;

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
       List.fold_left
         (fun t ->
            function
              | Library (cs, bs, lib) ->
                  begin
                    let lib_name =
                      bs.bs_path^"/"^cs.cs_name
                    in

                    let lib_paths = 
                      SetString.elements
                       (set_string_of_list
                          (bs.bs_path
                           ::
                           List.rev_map
                             FilePath.UnixPath.dirname
                             (List.rev_map
                                (FilePath.UnixPath.concat bs.bs_path)
                                lib.lib_modules)))
                    in

                      {
                        lib_ocaml = 
                          (lib_name, 
                           lib_paths, 
                           extern cs.cs_data) :: t.lib_ocaml;
                        
                        lib_c = 
                          (* Extract OCaml library/clib *)
                          if bs.bs_c_sources <> [] then
                            (cs.cs_name, bs.bs_path) :: t.lib_c
                          else
                            t.lib_c}
                  end

              | Executable (cs, bs, exec) ->
                  begin

                    {t with

                         lib_c = 
                           (* Extract OCaml executable/clib *)
                           if bs.bs_c_sources <> [] then
                             (cs.cs_name, 
                              OASISExecutable.exec_main_path (cs, bs, exec)) 
                             :: 
                             t.lib_c
                           else
                             t.lib_c}
                  end

              | Flag _ | SrcRepo _ | Test _ ->
                  t)
         {lib_ocaml = []; lib_c = []}
         (* To preserve build order *)
         (List.rev pkg.sections)
     in
     let content = 
       [
         BaseData.basesysenvironment_ml;
         OCamlbuildData.myocamlbuild_ml;
         (
           Format.fprintf Format.str_formatter
             "@[<hv2>let package_default =@ %a@,@];;"
             (pp_odn ~opened_modules:[]) 
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


let plugin_main pkg =
    {
      moduls       = [OCamlbuildData.ocamlbuildsys_ml];
      setup        = func build "OCamlbuildPlugin.build";
      clean        = Some (func clean "OCamlbuildPlugin.clean");
      distclean    = None;
      other_action = create_ocamlbuild_files pkg;
    },
    OASISPackage.add_build_tool ~no_test:true 
      (ExternalTool "ocamlbuild") 
      pkg

let () =
  plugin_register 
    plugin_id 
    (Build plugin_main)
