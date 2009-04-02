
(** Message to user
    @author Sylvain Le Gall
  *)

(** Print a warning message 
  *)
let warn str =
  prerr_endline str
;;

(** Print an error message and exit.
  *)
let error str =
  prerr_endline str;
  exit 1
;;

(** Print information message.
  *)
let info str = 
  print_endline str
;;

(** Print begin of line when checking for a feature.
  *)
let checking str =
  print_string "checking for ";
  print_string str;
  print_string "... ";
  flush stdout
;;

(** Print end of line when checking for a feature.
  *)
let result str =
  print_endline str;
  flush stdout
;;

(** Print result and return it.
  *)
let result_wrap str =
  result str;
  str
;;

