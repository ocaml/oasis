
(** Generate ocamlbuild build system
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open BaseGenerate;;
open BaseFileGenerate;;
open BaseUtils;;
open BaseExpr;;
open BaseGenCode;;
open BaseMessage;;
open CommonGettext;;

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
    (* Generate a files for library *)
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

    (* Generate myocamlbuild.ml *)
    file_generate 
      "myocamlbuild.ml"
      comment_ml
      (NeedSplit OCamlbuildData.myocamlbuild_ml)
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

generator_register
  Build
  "ocamlbuild"
  build
;;
