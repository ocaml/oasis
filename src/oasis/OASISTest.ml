
(** Test schema and generator
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISSchema;;
open OASISValueParser;;
open CommonGettext;;

let schema, generator =
  let schm =
   schema "Test"
  in
  let typ =
    new_field schm "Type"
      ~default:"none"
      string_not_empty
      (s_ "Plugin to use to run test.")
  in
  let command = 
    new_field schm "Command"
      string_not_empty
      (s_ "Command to run for the test.")
  in
  let working_directory =
    new_field schm "WorkingDirectory" 
      ~default:None
      (opt string_not_empty)
      (s_ "Directory to run the test.")
  in
  let run = 
    new_field_conditional schm "Run"
      ~default:true
      boolean
      (s_ "Enable this test.")
  in
  let build_tools = 
    OASISUtils.build_tools_fields schm
  in
    schm,
    (fun (_: string) wrtr ->
       {
         test_type              = typ wrtr;
         test_command           = command wrtr;
         test_working_directory = working_directory wrtr;
         test_run               = run wrtr;
         test_build_tools       = build_tools wrtr;
         test_schema_data       = wrtr;
       })
;;
