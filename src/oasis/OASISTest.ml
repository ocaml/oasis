
(** Test schema and generator
    @author Sylvain Le Gall
  *)

open OASISTypes

(* END EXPORT *)

open OASISSchema
open OASISValues
open OASISUtils
open CommonGettext
open PropList.Field

let schema, generator =
  let schm =
   schema "Test"
  in
  let typ =
    new_field schm "Type"
      ~default:"none"
      string_not_empty
      (fun () ->
         s_ "Plugin to use to run test.")
  in
  let command = 
    new_field schm "Command"
      command_line
      (fun () ->
         s_ "Command to run for the test.")
  in
  let working_directory =
    new_field schm "WorkingDirectory" 
      ~default:None
      (opt string_not_empty)
      (fun () ->
         s_ "Directory to run the test.")
  in
  let run = 
    new_field_conditional schm "Run"
      ~default:true
      boolean
      (fun () ->
         s_ "Enable this test.")
  in
  let build_tools = 
    build_tools_fields schm
  in
    schm,
    (fun (_: string) data ->
       {
         test_type              = typ data;
         test_command           = command data;
         test_working_directory = working_directory data;
         test_run               = run data;
         test_build_tools       = build_tools data;
         test_schema_data       = data;
       })
