
(** Check an _oasis file
    @author Sylvain Le Gall
  *)

open MainGettext
open SubCommand

let main () = 
  let _pkg: OASISTypes.package = 
    OASISParse.from_file
      ~ctxt:!BaseContext.default
      ~ignore_plugins:!ArgCommon.ignore_plugins
      !ArgCommon.oasis_fn
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
           (ArgCommon.ignore_plugins_specs 
            @
            ArgCommon.oasis_fn_specs)} 

let () = 
  SubCommand.register scmd
