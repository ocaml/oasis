
(** Various utilities for OASIS.
  *)

open OASISTypes;;
open OASISSchema;;
open OASISValueParser;;
open CommonGettext;;

let std_field nm comp_dflt schm = 
  let buildable = 
    new_field_conditional schm "buildable"
      ~default:true
      boolean
      (Printf.sprintf 
         (f_ "Set if the %s should be built. Use with flag.")
         nm)
  in
  let installable =
    new_field_conditional schm "installable"
      ~default:true
      boolean
      (Printf.sprintf
         (f_ "Set if the %s should be distributed.")
         nm)
  in
  let compiled_object =
    new_field schm "compiledobject"
      ~default:comp_dflt
      compiled_object
      (Printf.sprintf 
         (f_ "Define the compilation type of %s: byte, native or best")
         nm)
  in
    buildable, 
    installable,
    compiled_object
;;

