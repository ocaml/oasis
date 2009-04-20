
(** Library schema and generator 
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISSchema;;
open OASISValueParser;;

let schema, generator =
  let schm =
    schema ()
  in
  let path =
    new_field schm "path" directory_exists
  in
  let buildable = 
    new_field_conditional schm "buildable"
      ~default:true
      boolean
  in
  let modules =
    new_field schm "modules" 
      ~default:[]
      modules
  in
  let compiled_object =
    new_field schm "compiledobject"
      ~default:Best
      compiled_object
  in
    schm,
    (fun wrtr ->
       {
         lib_buildable       = buildable wrtr;
         lib_path            = path wrtr;
         lib_modules         = modules wrtr;
         lib_compiled_object = compiled_object wrtr;
         lib_extra           = extra wrtr;
       })
;;
