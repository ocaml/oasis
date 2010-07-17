
(** Global context for OASIS, include message management and i18n
    @author Sylvain Le Gall
  *)

open OASISGettext 

type t =
  {
    verbose: bool;
    debug:   bool;
  }

let default =
  ref 
    {
      verbose = true;
      debug   = false;
    }

let quiet = 
  {
    verbose = false;
    debug   = false;
  }


(** Command line arguments
  *)
let args () =
  ["-quiet",
   Arg.Unit (fun () -> default := {!default with verbose = false}),
   (s_ " Run quietly");

   "-debug",
   Arg.Unit (fun () -> default := {!default with debug = true}),
   (s_ " Output debug message")]
