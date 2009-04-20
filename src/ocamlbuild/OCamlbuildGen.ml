
(** Generate ocamlbuild build system
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open Format;;
open BaseGenerate;;
open BaseFileGenerate;;
open BaseUtils;;
open BaseExpr;;
open BaseExprTools;;

let build pkg =

  let pp_clean fmt () = 
    fprintf fmt "OCamlbuildBuild.clean ()"
  in

  let pp_choices_target fmt oasis_choices extra_choices target =
    fprintf fmt "%a, %S"
      (pp_code_expr_choices pp_print_bool) 
      ((of_oasis_choices oasis_choices) @ extra_choices)
      target
  in

  let pp_setup fmt () = 
    fprintf fmt 
      "@[<hv2>OCamlbuildBuild.build@ %a@]"
      (pp_ocaml_list (fun fmt e -> e fmt))
      (List.flatten 
         [
           List.map
             (fun (nm, lib) fmt ->

                let byte cond =
                  pp_choices_target fmt
                    lib.lib_buildable 
                    cond
                    (Filename.concat lib.lib_path (nm^".cma"))
                in

                let native cond = 
                  pp_choices_target fmt
                    lib.lib_buildable
                    cond
                    (Filename.concat lib.lib_path (nm^".cmxa"))
                in

                  match lib.lib_compiled_object with 
                    | Byte ->
                        byte []
                    | Native ->
                        byte []; 
                        fprintf fmt ";@ "; 
                        native []
                    | Best -> 
                        byte []; 
                        fprintf fmt ";@ "; 
                        native [Test ("ocamlbest", "byte"), false])
             pkg.libraries;

           List.map 
             (fun (nm, exec) fmt ->

                let byte cond = 
                  pp_choices_target fmt
                    exec.exec_buildable
                    cond
                    ((Filename.chop_extension exec.exec_main_is)^".byte");
                in

                let native cond = 
                  pp_choices_target fmt
                    exec.exec_buildable
                    cond
                    ((Filename.chop_extension exec.exec_main_is)^".native")
                in

                  match exec.exec_compiled_object with
                    | Byte ->
                        byte []
                    | Native ->
                        native []
                    | Best ->
                        byte [Test ("ocamlbest", "native"), false];
                        fprintf fmt ";@ ";
                        native [Test ("ocamlbest", "byte"), false])
             pkg.executables;
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
      pp_setup_fun     = pp_setup;
      pp_clean_fun     = Some pp_clean;
      pp_distclean_fun = None;
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
