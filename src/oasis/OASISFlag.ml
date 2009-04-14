
(** Flag schema and generator
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISSchema;;
open OASISValueParser;;

let schema, generator = 
  let schm =
    schema ()
  in
  let descr = 
    new_field schm "description" 
      ~default:None 
      (opt string_not_empty)
  in
  let default = 
    new_field_conditional schm "default" 
      ~default:true
      boolean
  in
    schm,
    (fun wrtr ->
       {
         flag_description = descr wrtr;
         flag_default     = default wrtr;
         flag_extra       = extra wrtr;
       })
;;
