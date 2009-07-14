
(** Executable schema and generator
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISSchema;;
open OASISValueParser;;

let schema, generator =
  let schm =
    schema ()
  in
  let main_is =
    new_field schm "mainis" 
      (fun ctxt vl ->
         str_regexp
           (Str.regexp ".*\\.ml$")
           ".ml file"
           ctxt
           (file_exists ctxt vl))
  in
  let buildable =
    new_field_conditional schm "buildable"
      ~default:true
      boolean
  in
  let installable =
    new_field_conditional schm "installable"
      ~default:true
      boolean
  in
  let compiled_object =
    new_field schm "compiledobject"
      ~default:Byte
      compiled_object
  in
    schm,
    (fun wrtr -> 
       {
         exec_buildable       = buildable wrtr;
         exec_installable     = installable wrtr;
         exec_main_is         = main_is wrtr;
         exec_extra           = extra wrtr;
         exec_compiled_object = compiled_object wrtr;
       })
;;

