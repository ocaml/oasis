
(** Message to user
    @author Sylvain Le Gall
  *)

let verbose =
  ref true
;;

(** Print a warning message 
  *)
let warning str =
  if !verbose then
    prerr_endline str
;;

(** Print an error message and exit.
  *)
let error str =
  if !verbose then 
    prerr_endline str;
  exit 1
;;

(** Print information message.
  *)
let info str = 
  if !verbose then
    Printf.printf "%s\n%!" str
;;

(** Print begin of line when checking for a feature.
  *)
let checking str =
  if !verbose then
    Printf.printf "checking for %s... %!" str
;;

(** Print end of line when checking for a feature.
  *)
let result str =
  if !verbose then
    Printf.printf "%s\n%!" str
;;

(** Print result and return it.
  *)
let result_wrap str =
  result str;
  str
;;

