
(** Executable schema and generator
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISSchema;;
open OASISValueParser;;
open CommonGettext;;

let schema, generator =
  let schm =
    schema "executable" 
  in
  let main_is =
    new_field schm "mainis" 
      (fun ctxt vl ->
         str_regexp
           (Str.regexp ".*\\.ml$")
           (s_ ".ml file")
           ctxt
           (file_exists ctxt vl))
      (s_ "OCaml file (.ml) containing main procedure for the executable.")
  in
  let buildable, installable, compiled_object = 
    OASISUtils.std_field (s_ "executable") Byte schm
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

