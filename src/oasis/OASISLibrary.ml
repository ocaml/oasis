
(** Library schema and generator 
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISSchema;;
open OASISValueParser;;
open CommonGettext;;

let schema, generator =
  let schm =
    schema "library"
  in
  let path =
    new_field schm "path" 
      directory_exists
      (s_ "Directory containing the library")
  in
  let modules =
    new_field schm "modules" 
      ~default:[]
      modules
      (s_ "List of modules to compile.") 
  in
  let buildable, installable, compiled_object = 
    OASISUtils.std_field (s_ "library") Best schm
  in
    schm,
    (fun wrtr ->
       {
         lib_buildable       = buildable wrtr;
         lib_installable     = installable wrtr;
         lib_path            = path wrtr;
         lib_modules         = modules wrtr;
         lib_compiled_object = compiled_object wrtr;
         lib_extra           = extra wrtr;
       })
;;
