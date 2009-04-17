
(** Most standard variables for OCaml 
    @author Sylvain Le Gall
  *)

open BaseCheck;;

let ocamlc = prog_opt "ocamlc";;

let ocamlc_config = BaseOCamlcConfig.var_cache ocamlc;;

let ocaml_version = ocamlc_config "ocaml_version" ;; 
let os_type       = ocamlc_config "os_type";;

let ocamlopt      = prog_opt "ocamlopt";;

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

(** Compute the default suffix for link (host OS dependent)
  *)
let suffix_link =
  Env.var_cache "suffix_link"
    (fun env ->
       (match Sys.os_type with
          | "Win32" -> ".lnk"
          | _ -> ""),
       env
    )
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

(** Return ocaml version and check against a minimal version.
  *)
let ocaml_version version_cmp env = 
  version 
    "ocaml version" 
    "ocaml" 
    version_cmp 
    ocaml_version
;;

