
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

let build pkg =

  let fn_itarget =
    pkg.name^".itarget"
  in
  let fn_otarget =
    pkg.name^".otarget"
  in

  let pp_clean fmt () = 
    fprintf fmt "OCamlbuildBuild.clean ()"
  in

  let pp_setup fmt () = 
    fprintf fmt "OCamlbuildBuild.build %S"
      fn_otarget
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

    (* Generates toplevel .itarget *)
    file_generate
      fn_itarget
      comment_ocamlbuild
      (Split
         (
           [],
           List.flatten
             [
               List.map
                 (fun (nm, lib) -> Filename.concat lib.lib_path (nm^".cma"))
                 pkg.libraries;
               List.map
                 (fun (nm, lib) -> Filename.concat lib.lib_path (nm^".cmxa"))
                 pkg.libraries;
               List.map
                 (fun (nm, exec) -> (Filename.chop_extension exec.exec_main_is)^".byte")
                 pkg.executables
             ],
           []
         )
      );

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
    },
    {pkg with build_tools = "ocamlbuild" :: pkg.build_tools}
;;

generator_register
  Build
  "ocamlbuild"
  build
;;
