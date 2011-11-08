

(** Display version of the OASIS command
  *)

open SubCommand
open OASISGettext

let main () = 
  print_endline (OASISVersion.string_of_version OASISConf.version_full)

let scmd = 
  SubCommand.make
    "version"
    (s_ "Display the version of the OASIS program running")
    CLIData.version_mkd
    main

let () = 
  SubCommand.register scmd
