
(** Generate ocamlbuild build system
    @author Sylvain Le Gall
  *)

(*
           (match Sys.os_type with
              | "Win32" ->
                  "-classic-display -no-log -byte-plugin -install-lib-dir "^
                  (Filename.concat (Findlib.ocaml_stdlib ()) "ocamlbuild")
              | _ ->
                  ""
           ),
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

  let pp_setup fmt () = 
    fprintf fmt 
      "@[<hv2>OCamlbuildBuild.build@ %a@]"
      (pp_ocaml_list (fun fmt e -> e fmt))
      (List.flatten 
         [
           List.map
             (fun (nm, lib) fmt ->
                fprintf fmt "%a, %S"
                  (pp_code_expr_choices pp_print_bool) 
                  (of_oasis_choices lib.lib_buildable)
                  (Filename.concat lib.lib_path (nm^".cma")))
             pkg.libraries;
           List.map
             (fun (nm, lib) fmt ->
                fprintf fmt "%a, %S"
                  (pp_code_expr_choices pp_print_bool) 
                  (
                    (of_oasis_choices lib.lib_buildable)
                    @
                    [Test ("ocamlbest", "byte"), false]
                  )
                  (Filename.concat lib.lib_path (nm^".cmxa")))
             pkg.libraries;
           (* TODO: exec *)
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
    },
    {pkg with build_tools = "ocamlbuild" :: pkg.build_tools}
;;

generator_register
  Build
  "ocamlbuild"
  build
;;
