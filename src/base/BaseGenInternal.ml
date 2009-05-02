
open OASISTypes;;
open Format;;
open BaseGenerate;;
open BaseUtils;;

(* Configuration *)
let configure pkg standard_vars =

  let pp_print_checks fmt pkg = 
    pp_ocaml_list
      (fun fmt e -> e fmt)
      fmt
      (List.flatten 
         [
           List.map 
             (fun e fmt ->
                match e with
                  | pkg, Some ver -> 
                      fprintf fmt
                        "@[<hv2>BaseCheck.package@ ~version_comparator:%S@ %S@]"
                        pkg
                        ver
                  | pkg, None ->
                      fprintf fmt
                        "@[<hv2>BaseCheck.package@, %S@]"
                        pkg)
             pkg.build_depends;
           List.map
             (fun e fmt ->
                fprintf fmt "BaseCheck.prog %S" e)
             pkg.build_tools;

           List.map
             (fun e fmt ->
                let var = 
                  match e with 
                    | SVocamlc -> "ocamlc"
                    | SVocamlopt -> "ocamlopt"
                    | SVocamlbest -> "ocamlbest"
                    | SVsuffix_program -> "suffix_program"
                    | SVocaml_version -> "ocaml_version"
                    | SVstandard_library_default -> "standard_library_default"
                    | SVstandard_library -> "standard_library"
                    | SVstandard_runtime -> "standard_runtime"
                    | SVccomp_type -> "ccomp_type"
                    | SVbytecomp_ccompiler -> "bytecomp_ccompiler"
                    | SVbytecomp_c_linker -> "bytecomp_c_linker"
                    | SVbytecomp_c_libraries -> "bytecomp_c_libraries"
                    | SVnative_c_compiler -> "native_c_compiler"
                    | SVnative_c_linker -> "native_c_linker"
                    | SVnative_c_libraries -> "native_c_libraries"
                    | SVnative_partial_linker -> "native_partial_linker"
                    | SVranlib -> "ranlib"
                    | SVcc_profile -> "cc_profile"
                    | SVarchitecture -> "architecture"
                    | SVmodel -> "model"
                    | SVsystem -> "system"
                    | SVext_obj -> "ext_obj"
                    | SVext_asm -> "ext_asm"
                    | SVext_lib -> "ext_lib"
                    | SVext_dll -> "ext_dll"
                    | SVos_type -> "os_type"
                    | SVdefault_executable_name -> "default_executable_name"
                    | SVsysthread_supported -> "systhread_supported"
                in
                  fprintf fmt "BaseStandardVar.%s" var)
             standard_vars;
         ])
  in

  let pp_print_args =
    pp_ocaml_list
      (fun fmt (nm, flg) ->
         fprintf fmt 
           "@[<hv>BaseArgExt.enable@, %S@, %S@, %a@]"
           nm 
           (match flg.flag_description with
              | Some hlp -> hlp
              | None -> "")
           (pp_ocaml_list
              (fun fmt (expr, vl) ->
                 fprintf fmt "%a,@ %B"
                   BaseExprTools.pp_code_expr 
                   (BaseExprTools.of_oasis expr)
                   vl))
           flg.flag_default)
  in

  let pp_print_files_ab =
    pp_ocaml_list pp_ocaml_string
  in     

  let pp_gen fmt () = 
    fprintf fmt
      "@[<hv2>BaseConfigure.configure@ %S@ %S@ %a@ %a@ %a@]"
      pkg.name
      pkg.version
      pp_print_args pkg.flags
      pp_print_checks pkg
      pp_print_files_ab pkg.files_ab
  in

    {
      moduls           = [BaseData.basesys_ml];
      pp_setup_fun     = pp_gen;
      pp_clean_fun     = None;
      pp_distclean_fun = None;
      other_action     = (fun _ -> ());
      files_generated  = (List.map BaseFileAB.to_filename pkg.files_ab);
      standard_vars    = [];
    }
;;

configure_generator_register
  "autobuild"
  configure
;;

open BaseInstallTools;;

(* Installation *)
let install pkg =

  let pp_gen fmt () =

    pp_ocaml_expr fmt
      (APP 
         ("BaseInstall.install",
          [LST (List.map library_code_of_oasis pkg.libraries);
           LST (List.map executable_code_of_oasis pkg.executables)]))
  in

    {
      moduls           = [BaseData.basesys_ml];
      pp_setup_fun     = pp_gen;
      pp_clean_fun     = None;
      pp_distclean_fun = None;
      other_action     = (fun _ -> ());
      files_generated  = [];
      standard_vars    = [];
    },
    pkg
;;

generator_register
  Install
  "autobuild"
  install
;;

