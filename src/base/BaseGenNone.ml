
(** Plugin to handle "none" generation
    @author Sylvain Le Gall
  *)

open BaseGenerate;;
open BaseGenCode;;

let no_generate knd data =
  {
    moduls           = [];
    setup_code       = FUN
                         (["_"; "_"],
                          [APP 
                             ("failwith",
                              [STR
                                 ("No implementation for "^
                                  (string_of_generator_kind knd))])]);
    clean_code       = [];
    distclean_code   = [];
    other_action     = ignore;
    files_generated  = [];
    standard_vars    = [];
  },
  data
;;

List.iter
  (fun knd -> 
     generator_register 
       knd
       "none"
       (no_generate knd))
  [Build; Doc; Test; Install;]
;;

configure_generator_register
  "none"
  (fun pkg standard_vars ->
     {
       (fst (no_generate Build pkg)) with 
           setup_code = FUN
                          (["_"; "_"],
                           [APP
                              ("failwith",
                               [STR "No implementation for configure"])]);
     })
;;

