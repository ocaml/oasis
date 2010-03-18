
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

(**/**)
let generic_message ?(after=ignore) cond beg fmt =
  if cond then
    begin
      Printf.fprintf stderr "%s: " beg;
      Printf.kfprintf 
        (fun chn -> 
           Printf.fprintf chn "\n%!";
           after ())
        stderr
        fmt
    end
  else
    begin
      Printf.ifprintf 
        stderr
        fmt
    end
(**/**)


(** Print a debug message
  *)
let debug fmt =
  generic_message !debug "D" fmt

(** Print information message.
  *)
let info fmt = 
  generic_message !verbose "I" fmt

(** Print a warning message 
  *)
let warning fmt =
  generic_message !verbose "W" fmt

(** Print an error message and exit.
  *)
let error fmt =
  generic_message ~after:(fun () -> exit 1) !verbose "E" fmt
