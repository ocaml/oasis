
(** Generate ocamlbuild build system
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open BasePlugin;;
open BaseFileGenerate;;
open BaseUtils;;
open BaseExpr;;
open BaseGenCode;;
open BaseMessage;;
open CommonGettext;;

let plugin_id =
  "ocamlbuild"
;;

let build pkg =

  let clean_code = 
    APP ("OCamlbuildBuild.clean", [], [UNT])
  in

  let code_choices_target oasis_choices extra_choices target =
    TPL
      [
        code_of_bool_choices 
          ((choices_of_oasis oasis_choices) @ extra_choices);
        STR target
      ]
  in

  let setup_code = 
    APP
      ("OCamlbuildBuild.build",
       [],
       [
         LST
           (List.flatten 
              [
                List.fold_left
                  (fun acc (nm, lib) ->

                     let byte cond =
                       code_choices_target
                         lib.lib_build 
                         cond
                         (Filename.concat lib.lib_path (nm^".cma"))
                     in

                     let native cond = 
                       code_choices_target
                         lib.lib_build
                         cond
                         (Filename.concat lib.lib_path (nm^".cmxa"))
                     in

                       match lib.lib_compiled_object with 
                         | Byte ->
                             byte [] :: acc
                         | Native ->
                             byte [] :: native [] :: acc
                         | Best -> 
                             byte []
                             ::
                             native [Test ("ocamlbest", "byte"), false]
                             ::
                             acc)
                  []
                  pkg.libraries;

                List.fold_left
                  (fun acc (nm, exec) ->

                     let byte cond = 
                       code_choices_target
                         exec.exec_build
                         cond
                         ((Filename.chop_extension exec.exec_main_is)^".byte");
                     in

                     let native cond = 
                       code_choices_target
                         exec.exec_build
                         cond
                         ((Filename.chop_extension exec.exec_main_is)^".native")
                     in

                       match exec.exec_compiled_object with
                         | Byte ->
                             byte [] :: acc
                         | Native ->
                             native [] :: acc
                         | Best ->
                             byte [Test ("ocamlbest", "native"), false]
                             ::
                             native [Test ("ocamlbest", "byte"), false]
                             ::
                             acc)
                  []
                  pkg.executables;
              ])
       ])
  in

  let other_action () = 

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

      let target_exec dir name comp =
        let ext =
          match comp with 
            | Best ->
                "{native,byte}"
            | Byte -> 
                "byte"
            | Native ->
                "native"
        in
          Printf.sprintf "<%s/%s.%s>" dir name ext
      in

      let rev_content = 
        (* Extract content for libraries *)
        List.fold_left 
          (fun acc (nm, lib) ->
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
             let acc = 
               (Printf.sprintf "# Library %s" nm) :: acc
             in
               SetString.fold
                 (fun dir ->
                    tags_of_build_depends 
                      (target_ml dir) 
                      (lib.lib_build_depends @ pkg.build_depends))
                 all_path
                 acc)
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
             let acc = 
               (Printf.sprintf "# Executable %s" nm) :: acc
             in
             let acc =
               tags_of_build_depends
                 (target_ml dir)
                 (exec.exec_build_depends @ pkg.build_depends)
                 acc
             in
               tags_of_build_depends
                 (target_exec dir nm exec.exec_compiled_object)
                 (exec.exec_build_depends @ pkg.build_depends)
                 acc)
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

    (* Generate files for OCaml library (.mllib) *)
    List.iter
      (fun (nm, lib) ->
         let fn_base =
           Filename.concat lib.lib_path nm
         in

         (* Generate .mllib files *)
         let fn_mllib =
           fn_base ^ ".mllib"
         in
         let () =
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

           ())
      pkg.libraries;

    (* Generate _tags *)
    file_generate
      "_tags"
      comment_ocamlbuild
      (Split
         ([],
          tags_of_package pkg,
          []));

    (* Generate myocamlbuild.ml *)
    file_generate 
      "myocamlbuild.ml"
      comment_ml
      (let myocamlbuild_t = 
         REC
           ("OCamlAutobuild",
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
                    pkg.libraries))
            ])
       in
       let content = 
         OCamlbuildData.myocamlbuild_ml
         @
         [
           (
             Format.fprintf Format.str_formatter
               "@[<hv2>let package_default =@ %a@,@];;"
               pp_ocaml_expr myocamlbuild_t;
             Format.flush_str_formatter ()
           );
           "";
           "let dispatch_default = \
                  OCamlAutobuild.dispatch_default package_default;;"; 
           "";
         ]
        in
          Split ([], 
                 content, 
                 ["Ocamlbuild_plugin.dispatch dispatch_default;;"]))
  in
    {
      moduls = 
        [
          BaseData.basesys_ml;
          OCamlbuildData.ocamlbuildsys_ml;
        ];
      setup_code       = setup_code;
      clean_code       = [clean_code];
      distclean_code   = [];
      other_action     = other_action;
      files_generated  = [];
      standard_vars    = [SVocamlbest; SVos_type; SVstandard_library];
    },
    {pkg with build_tools = "ocamlbuild" :: pkg.build_tools}
;;

plugin_register plugin_id (Build build);;
