
(** Various utilities for OASIS.
  *)

open OASISTypes;;
open OASISSchema;;
open OASISValueParser;;
open CommonGettext;;

let std_field nm comp_dflt schm = 
  let build = 
    new_field_conditional schm "Build"
      ~default:true
      boolean
      (Printf.sprintf 
         (f_ "Set if the %s should be built. Use with flag.")
         nm)
  in
  let install =
    new_field_conditional schm "Install"
      ~default:true
      boolean
      (Printf.sprintf
         (f_ "Set if the %s should be distributed.")
         nm)
  in
  let compiled_object =
    new_field schm "CompiledObject"
      ~default:comp_dflt
      compiled_object
      (Printf.sprintf 
         (f_ "Define the compilation type of %s: byte, native or best")
         nm)
  in
    build, 
    install,
    compiled_object
;;

let depends_field schm = 
  let build_depends = 
    new_field schm "builddepends" 
      ~default:[]
      build_depends
      (s_ "Dependencies on findlib packages and internal libraries.")
  in
  let build_tools =
    new_field schm "buildtools"
      ~default:[]
      comma_separated
      (s_ "Executables require to compile.")
  in
    build_depends, build_tools
;;
