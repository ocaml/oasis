
open OASISTypes;;
open BaseGenerate;;
open BaseUtils;;
open BaseGenCode;;

(* Configuration *)
let configure pkg standard_vars =

  let code_checks = 
    LST
      (List.flatten 
         [
           List.map 
             (fun (pkg, ver_opt) -> 
                let version_arg = 
                  match ver_opt with
                    | Some ver ->
                        let cmp = 
                          BaseVersion.comparator_of_string ver
                        in
                          [
                            "version_comparator",
                            TPL [STR ver; 
                                 BaseVersion.code_of_comparator cmp;
                                 STR (BaseVersion.varname_of_comparator cmp)];
                          ]
                    | None ->
                        []
                in
                  APP ("BaseCheck.package", version_arg, [STR pkg]))
             pkg.build_depends;

           List.map
             (fun e -> APP ("BaseCheck.prog", [], [STR e]))
             pkg.build_tools;

           List.map
             (fun e ->
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
                  VAR ("BaseStandardVar."^var))
             standard_vars;
         ])
  in

  let code_args =
    LST
      (List.map 
         (fun (nm, flg) ->
            APP
              ("BaseArgExt.enable",
               [],
               [
                 STR nm; 
                 STR 
                   (match flg.flag_description with
                      | Some hlp -> hlp
                      | None -> "");
                 BaseExpr.code_of_bool_choices
                   (BaseExpr.choices_of_oasis flg.flag_default)]))
         pkg.flags)
  in

  let code_files_ab =
    LST (List.map (fun f -> STR f) pkg.files_ab)
  in     

  let code = 
    APP
      ("InternalConfigure.configure",
       [],
       [
         STR pkg.name;
         STR pkg.version;
         code_args;
         code_checks;
         code_files_ab
       ])
  in

    {
      moduls = 
        [
          BaseData.basesys_ml; 
          InternalData.internalsys_ml
        ];
      setup_code       = code;
      clean_code       = [];
      distclean_code   = [];
      other_action     = (fun _ -> ());
      files_generated  = (List.map BaseFileAB.to_filename pkg.files_ab);
      standard_vars    = [];
    }
;;

configure_generator_register
  "autobuild"
  configure
;;

(* Installation *)
let install pkg =

  let code =
    APP 
      ("InternalInstall.install",
       [],
       [
         LST 
           (List.map 
              InternalInstall.library_code_of_oasis 
              pkg.libraries);
         LST 
           (List.map 
              InternalInstall.executable_code_of_oasis 
              pkg.executables);
       ]
      )
  in

    {
      moduls = 
        [
          BaseData.basesys_ml; 
          InternalData.internalsys_ml
        ];
      setup_code       = code;
      clean_code       = [];
      distclean_code   = [];
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

