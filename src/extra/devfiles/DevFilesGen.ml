
(** Generate standard development files
    @author Sylvain Le Gall
  *)

open BasePlugin;;
open BaseFileGenerate;;

let plugin_id = 
  "DevFiles"
;;

let main pkg = 
  (* Generate Makefile (for standard dev. env.) *)
  file_generate
    "Makefile"
    comment_sh
    (NeedSplit
       DevFilesData.makefile);

  (* Generate configure (for standard dev. env.) *)
  file_generate
    "configure"
    comment_sh
    (NeedSplit
       DevFilesData.configure);
  Unix.chmod "configure" 0o755;
;;

plugin_register plugin_id (Extra main);;
