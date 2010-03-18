
(** Handle command line argument
    @author Sylvain Le Gall
  *)

open OASISUtils
open OASISGettext

let parse argv args =
    (* Simulate command line for Arg *)
    let current =
      ref 0
    in

      try
        Arg.parse_argv
          ~current:current
          (Array.concat [[|"none"|]; argv])
          (Arg.align args)
          (failwithf1 (f_ "Don't know what to do with arguments: '%s'"))
          (s_ "configure options:")
      with 
        | Arg.Help txt ->
            print_endline txt;
            exit 0
        | Arg.Bad txt ->
            prerr_endline txt;
            exit 1
