
(** Message to user
    @author Sylvain Le Gall
  *)

let verbose =
  ref true
;;

(** Print a warning message 
  *)
let warning str =
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
  if !verbose then
    (
      print_endline str;
      flush stdout
    )
;;

(** Print begin of line when checking for a feature.
  *)
let checking str =
  if !verbose then
    (
      print_string "checking for ";
      print_string str;
      print_string "... ";
      flush stdout
    )
;;

(** Print end of line when checking for a feature.
  *)
let result str =
  if !verbose then
    (
      print_endline str;
      flush stdout
    )
;;

(** Print result and return it.
  *)
let result_wrap str =
  result str;
  str
;;

