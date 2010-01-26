
(** Plugin to handle "none" generation
    @author Sylvain Le Gall
  *)

let not_implemented str _ _ =
  failwith ("No implementation for "^str)

(* END EXPORT *)

open BasePlugin;;
open ODN;;

let plugin_id = "None";;

let no_generate str data =
  {
    moduls           = [NoneData.nonesys_ml];
    setup_code       = APP ("NonePlugin.not_implemented", [], [STR str]);
    clean_code       = None;
    distclean_code   = None;
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
    Install   (no_generate "install", no_generate "uninstall");
  ]
;;
