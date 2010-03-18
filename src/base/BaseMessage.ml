
(** Message to user
    @author Sylvain Le Gall
  *)

open OASISGettext

let verbose =
  ref true

let debug =
  ref false

(** Command line arguments
  *)
let args =
  ["-quiet",
   Arg.Clear verbose,
   (s_ " Run quietly");

   "-debug",
   Arg.Set debug,
   (s_ " Output debug message")]

(** Print a warning message 
  *)
let warning str =
  if !verbose then
    prerr_endline str

(** Print an error message and exit.
  *)
let error str =
  if !verbose then 
    prerr_endline str;
  exit 1

(** Print information message.
  *)
let info str = 
  if !verbose then
    Printf.printf "%s\n%!" str

let debug fmt =
  if !debug then
    Printf.fprintf  stderr fmt
  else
    Printf.ifprintf stderr fmt

(** Print begin of line when checking for a feature.
  *)
let checking str =
  if !verbose then
    Printf.printf "checking for %s... %!" str

(** Print end of line when checking for a feature.
  *)
let result str =
  if !verbose then
    Printf.printf "%s\n%!" str

(** Print result and return it.
  *)
let result_wrap str =
  result str;
  str

