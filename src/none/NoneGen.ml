
(** Plugin to handle "none" generation
    @author Sylvain Le Gall
  *)

open BasePlugin;;
open BaseGenCode;;

let plugin_id = "None";;

let no_generate str data =
  {
    moduls           = [];
    setup_code       = FUN
                         (["_"; "_"],
                          [APP 
                             ("failwith",
                              [],
                              [STR
                                 ("No implementation for "^str)])]);
    clean_code       = [];
    distclean_code   = [];
    other_action     = ignore;
    files_generated  = [];
  },
  data
;;

List.iter
  (plugin_register plugin_id)
  [
    Configure (fun pkg -> (fst (no_generate "configure" pkg)));
    Build     (no_generate "build"); 
    Doc       (no_generate "doc"); 
    Test      (no_generate "test"); 
    Install   (no_generate "install");
  ]
;;
