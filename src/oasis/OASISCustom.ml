
(** Custom command to run before/after specific actions
    @author Sylvain Le Gall
  *)

(* END EXPORT *)

open OASISSchema
open OASISValues
open OASISTypes

let add_fields schm nm hlp_pre hlp_post = 
  let pre_command = 
    new_field_conditional schm ("Pre"^nm^"Command")
      ~default:None
      (opt command_line)
      hlp_pre
  in
  let post_command = 
    new_field_conditional schm ("Post"^nm^"Command")
      ~default:None
      (opt command_line)
      hlp_post
  in
    (fun data -> 
       {
         pre_command  = pre_command data;
         post_command = post_command data;
       })

