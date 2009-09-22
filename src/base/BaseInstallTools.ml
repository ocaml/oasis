
(** BaseInstall tools
    @author Sylvain Le Gall
  *)

open BaseInstall;;
open BaseExprTools;;
open BaseGenCode;;

module OASIS = OASISTypes;;

let library_code_of_oasis (nm, lib) =
  REC 
    ("BaseInstall",
     ["lib_name",        STR nm;
      "lib_installable", code_of_bool_choices 
                           ((choices_of_oasis lib.OASIS.lib_buildable)
                           @
                            (choices_of_oasis lib.OASIS.lib_installable));
      "lib_modules",     LST (List.map 
                                (fun s -> STR s) 
                                lib.OASIS.lib_modules);
      "lib_path",        STR lib.OASIS.lib_path;
      "lib_extra",       LST []])
;;

let executable_code_of_oasis (nm, exec) = 
  REC 
    ("BaseInstall",
     ["exec_name",        STR nm;
      "exec_installable", code_of_bool_choices 
                            ((choices_of_oasis exec.OASIS.exec_buildable)
                            @
                             (choices_of_oasis exec.OASIS.exec_installable));
      "exec_path",        STR (Filename.dirname exec.OASIS.exec_main_is)])
;;

