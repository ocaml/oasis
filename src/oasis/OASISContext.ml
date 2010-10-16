
open OASISGettext 

type level =
  [ `Debug
  | `Info 
  | `Warning
  | `Error]

type t =
  {
    verbose:        bool;
    debug:          bool;
    ignore_plugins: bool;
    printf:         level -> string -> unit; 
  }

let printf lvl str = 
  let beg = 
    match lvl with 
      | `Error -> s_ "E: "
      | `Warning -> s_ "W: "
      | `Info  -> s_ "I: "
      | `Debug -> s_ "D: "
  in
    match lvl with 
      | `Error ->
          prerr_endline (beg^str)
      | _ ->
          print_endline (beg^str)

let default =
  ref 
    {
      verbose        = true;
      debug          = false;
      ignore_plugins = false;
      printf         = printf;
    }

let quiet = 
  {!default with 
       verbose = false;
       debug   = false;
  }


let args () =
  ["-quiet",
   Arg.Unit (fun () -> default := {!default with verbose = false}),
   (s_ " Run quietly");

   "-debug",
   Arg.Unit (fun () -> default := {!default with debug = true}),
   (s_ " Output debug message")]
