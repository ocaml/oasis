
(** Help on subcommands 
  *)

open SubCommand 
open ArgExt
open MainGettext

let scmd_name = 
  ref None

let main () = 
  let pp_print_help =
    match !scmd_name with 
      | None ->
          pp_print_help NoSubCommand
      | Some "all" ->
          pp_print_help AllSubCommand
      | Some nm ->
          pp_print_help (SubCommand nm)
  in
    pp_print_help Output Format.std_formatter ()

let scmd = 
  {(SubCommand.make
      "help"
      (s_ "Display help for a subcommand")
      CLIData.help_mkd
      main)
     with 
         scmd_usage = s_ "[subcommand|all]";
         scmd_anon  = (fun s -> scmd_name := Some s)}

let () = 
  SubCommand.register scmd
    
