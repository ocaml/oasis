
(** Flag schema and generator
    @author Sylvain Le Gall
  *)

open OASISTypes

(* END EXPORT *)

open OASISSchema
open OASISValues
open OASISUtils
open OASISGettext
open PropList.Field

let schema, generator = 
  let schm =
    schema "Flag" 
  in
  let cmn_section_gen =
    OASISSection.section_fields (s_ "flag") schm
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
    (fun nm data ->
       Flag
         (cmn_section_gen nm data,
          {
            flag_description = descr data;
            flag_default     = default data;
          }))
