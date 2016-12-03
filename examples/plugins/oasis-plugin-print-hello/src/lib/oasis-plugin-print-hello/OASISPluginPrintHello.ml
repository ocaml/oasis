
open CLISubCommand

let () =
  register
    "print-hello"
    "" (* synopsis *)
    "" (* help *)
    (make_run
       default_fspecs
       (fun ~ctxt:_ () -> print_endline "Hello!"))

