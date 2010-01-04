
(** Internal configure and install scheme for AutoBuild
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open BasePlugin;;
open BaseUtils;;
open BaseGenCode;;

let plugin_id = "Internal";;

plugin_register plugin_id (Configure InternalConfigure.plugin_main);;

(* Installation *)
let install pkg =

  let code =
    APP 
      ("InternalInstall.install",
       [],
       [
         LST 
           (List.map 
              InternalInstall.library_code_of_oasis 
              pkg.libraries);
         LST 
           (List.map 
              InternalInstall.executable_code_of_oasis 
              pkg.executables);
       ]
      )
  in

    {
      moduls = 
        [
          CommonData.commonsys_ml;
          BaseData.basesys_ml; 
          InternalData.internalsys_ml
        ];
      setup_code       = code;
      clean_code       = [];
      distclean_code   = [];
      other_action     = (fun _ -> ());
      files_generated  = [];
    },
    pkg
;;

(* Uninstall *)
let uninstall pkg = 
  {
    moduls = 
        [
          CommonData.commonsys_ml;
          BaseData.basesys_ml; 
          InternalData.internalsys_ml
        ];
    setup_code       = APP ("InternalInstall.uninstall", [], []);
    clean_code       = [];
    distclean_code   = [];
    other_action     = (fun _ -> ());
    files_generated  = [];
  },
  pkg
;;

plugin_register plugin_id 
  (Install (install, uninstall))
;;

