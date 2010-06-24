
(** Clean generated template files
    @author Sylvain Le Gall
  *)

open MainGettext
open SubCommand

let scmd = 
  SubCommand.make
    "setup-clean"
    (s_ "Clean all template files from their content")
    CLIData.setup_clean_mkd
    (fun () -> failwith "Not implemented")
    

let () = 
  SubCommand.register scmd

