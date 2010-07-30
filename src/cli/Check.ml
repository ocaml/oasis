
(** Check an _oasis file
    @author Sylvain Le Gall
  *)

open MainGettext
open SubCommand

let ignore_plugins =
  ref false

let main () = 
  let _pkg: OASISTypes.package = 
    OASISParse.from_file
      ~ctxt:!BaseContext.default
      ~ignore_plugins:!ignore_plugins
      !Setup.oasis_fn
  in
    ()

let scmd =
  {(SubCommand.make 
      ~std_usage:true
      "check"
      (s_ "Check an _oasis file")
      CLIData.check_mkd
      main) 
     with 
         scmd_specs =
           [
             "-ignore-plugins",
             Arg.Set ignore_plugins,
             s_ " Ignore plugin's field.";
           ]}

let () = 
  SubCommand.register scmd
