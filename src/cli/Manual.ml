
(** Display the manual
    @author Sylvain Le Gall
  *)

open MainGettext
open SubCommand

let main () = 
  OASISHelp.pp_print_help 
    Format.std_formatter 

    (* CLI help *)
    (ArgExt.pp_print_help ArgExt.AllSubCommand ArgExt.Markdown)

    (* Fields from schema *)
    BaseEnv.schema

    (* Environment variable *)
    (let lst = 
       BaseEnv.var_all ()
     in
       fun nm _ ->
         List.mem nm lst)

let scmd = 
  SubCommand.make
    "manual"
    (s_ "Display user manual")
    CLIData.manual_mkd
    main

let () = 
  SubCommand.register scmd
