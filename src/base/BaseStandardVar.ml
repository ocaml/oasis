
(** Most standard variables for OCaml 
    @author Sylvain Le Gall
  *)

open BaseCheck;;

let ocamlc   = prog_opt "ocamlc";;
let ocamlopt = prog_opt "ocamlopt";;

let (ocaml_version,
     standard_library_default,
     standard_library,
     standard_runtime,
     ccomp_type,
     bytecomp_c_compiler,
     bytecomp_c_linker,
     bytecomp_c_libraries,
     native_c_compiler,
     native_c_linker,
     native_c_libraries,
     native_partial_linker,
     ranlib,
     cc_profile,
     architecture,
     model,
     system,
     ext_obj,
     ext_asm,
     ext_lib,
     ext_dll,
     os_type,
     default_executable_name,
     systhread_supported) =
  let c = 
    BaseOCamlcConfig.var_cache ocamlc
  in
    c "version",
    c "standard_library_default",
    c "standard_library",
    c "standard_runtime",
    c "ccomp_type",
    c "bytecomp_c_compiler",
    c "bytecomp_c_linker",
    c "bytecomp_c_libraries",
    c "native_c_compiler",
    c "native_c_linker",
    c "native_c_libraries",
    c "native_partial_linker",
    c "ranlib",
    c "cc_profile",
    c "architecture",
    c "model",
    c "system",
    c "ext_obj",
    c "ext_asm",
    c "ext_lib",
    c "ext_dll",
    c "os_type",
    c "default_executable_name",
    c "systhread_supported"
;;

(** Check what is the best target for platform (opt/byte)
  *)
let ocamlbest =
  Env.var_cache "ocamlbest"
    (fun env ->
       try
         "native", snd (ocamlopt env)
       with Not_found ->
         "byte", snd (ocamlc env))
;;

(** Compute the default suffix for program (target OS dependent)
  *)
let suffix_program =
  Env.var_cache "suffix_program"
    (fun env ->
       let os_type, env =
         os_type env
       in
         (match os_type with 
            | "Win32" -> ".exe" 
            | _ -> ""
         ),
         env
    )
;;

(** Check against a minimal version.
  *)
let ocaml_version_constraint version_cmp env = 
  version 
    "ocaml version constraint" 
    "ocaml" 
    version_cmp 
    ocaml_version
;;
