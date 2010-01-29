
(** Handle command line argument
    @author Sylvain Le Gall
  *)

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
          (fun str -> 
             failwith 
               ("Don't know what to do with arguments: '"^str^"'"))
          "configure options:"
      with Arg.Help txt | Arg.Bad txt ->
        BaseMessage.error txt
