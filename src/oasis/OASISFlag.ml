
(** Flag schema and generator
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISSchema;;
open OASISValues;;
open OASISUtils;;
open CommonGettext;;
open PropList.Field;;

let schema, generator = 
  let schm =
    schema "flag" 
  in
  let descr = 
    new_field schm "Description" 
      ~default:None 
      (opt string_not_empty)
      (fun () -> 
         s_ "Help for the flag")
  in
  let default = 
    new_field_conditional schm "Default" 
      ~default:true
      boolean
      (fun () ->
         s_ "Default value for the flag")
  in
    schm,
    (fun (_: string) data ->
       {
         flag_description = descr data;
         flag_default     = default data;
         flag_schema_data = data;
       })
;;
