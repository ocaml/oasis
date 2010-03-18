
(** Build using ocamlbuild  
    @author Sylvain Le Gall
  *)

open OASISTypes
open OASISGettext
open BaseEnv
open BaseStandardVar

type target =
  | Std of string 
  | StdRename of string * string

let cond_targets_hook =
  ref (fun lst -> lst)

let build pkg argv =

  (* Return the filename in build directory *)
  let in_build_dir fn =
    Filename.concat 
      (OCamlbuildCommon.build_dir argv) 
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
                   List.filter
                     (fun fn ->
                        ends_with ".cma" fn ||
                        ends_with ".cmxa" fn ||
                        ends_with (ext_lib ()) fn ||
                        ends_with (ext_dll ()) fn)
                     unix_files
                 in

                   match tgts with 
                     | hd :: tl ->
                         (evs, Std hd)
                         :: 
                         (List.map (fun tgt -> [], Std tgt) tl)
                         @
                         acc
                     | [] ->
                         failwith 
                           (Printf.sprintf
                              (f_ "No possible ocamlbuild targets \
                                   in generated fiels %s for library %s")
                              (String.concat (s_ ", " ) tgts)
                              cs.cs_name)
               end

           | Executable (cs, bs, exec) when var_choose bs.bs_build ->
               begin
                 let evs, unix_exec_is, unix_dll_opt =
                   BaseBuilt.of_executable 
                     in_build_dir_of_unix
                     (cs, bs, exec)
                 in

                 let host_exec_is = 
                   in_build_dir_of_unix unix_exec_is
                 in

                 let target ext =
                   let unix_tgt = 
                     (BaseFilePath.Unix.concat
                        bs.bs_path
                        (BaseFilePath.Unix.chop_extension 
                           exec.exec_main_is))^ext
                   in

                     evs,
                     (if unix_tgt = unix_exec_is then
                        Std unix_tgt
                      else
                        StdRename (unix_tgt, host_exec_is))
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
      (fun fn ->
         if not (Sys.file_exists fn) then
           failwith 
             (Printf.sprintf 
                (f_ "Expected built file '%s' doesn't exist")
                fn))
      lst;
      (BaseBuilt.register bt bnm lst) 
  in

  (* Run a list of target + post process *)
  let run_ocamlbuild rtargets = 
    OCamlbuildCommon.run_ocamlbuild 
      (List.rev_map snd rtargets)
      argv;
    List.iter
      check_and_register
      (List.flatten (List.rev_map fst rtargets))
  in

  let last_rtargets =
    List.fold_left
      (fun acc (built, tgt) ->
         match tgt with 
           | Std nm -> 
               (built, nm) :: acc
           | StdRename (src, tgt) ->
               (* We run with a fake list for event registering *)
               run_ocamlbuild (([], src) :: acc);
               (* And then copy and register *)
               BaseFileUtil.cp 
                 (in_build_dir_of_unix src)
                 tgt;
               List.iter check_and_register built;
               [])
      []
      (!cond_targets_hook cond_targets)
  in
    if last_rtargets <> [] then
      run_ocamlbuild last_rtargets

let clean pkg extra_args  = 
  OCamlbuildCommon.run_clean extra_args;
  BaseBuilt.clean_all pkg

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

module PU = OASISPlugin.Build.Make(OCamlbuildId)

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
        | Library _ | Flag _ | Test _ | SrcRepo _ | Doc _ -> false
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

                 (* Generate .mllib files *)
                 let () =
                   let fn_base = 
                     FilePath.concat bs.bs_path cs.cs_name
                   in
                   let fn_generate ext =
                     file_generate
                       (FilePath.add_extension fn_base ext)
                       comment_ocamlbuild
                       (Split ([], lib.lib_modules, []));
                   in
                     if lib.lib_modules = [] then
                       warning 
                         (Printf.sprintf
                            (f_ "No module defined for library %s")
                            cs.cs_name);
                     fn_generate "mllib"
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

           | Flag _ | SrcRepo _ | Test _ | Doc _ ->
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
        (OCamlbuildDocPlugin.auto_doc_section pkg)
  in
    PU.register doit
