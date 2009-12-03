
(** Flag schema and generator
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISSchema;;
open OASISValueParser;;
open CommonGettext;;

let schema, generator = 
  let schm =
    schema "flag" 
  in
  let descr = 
    new_field schm "Description" 
      ~default:None 
      (opt string_not_empty)
      (s_ "Help for the flag")
  in
  let default = 
    new_field_conditional schm "Default" 
      ~default:true
      boolean
      (s_ "Default value for the flag")
  in
    schm,
    (fun (_: string) wrtr ->
       {
         flag_description = descr wrtr;
         flag_default     = default wrtr;
         flag_schema_data = wrtr;
       })
;;
