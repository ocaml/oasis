
(** Create the configure, build and install system 
    @author Sylvain Le Gall
  *)

open MainGettext
open OASISUtils
open SubCommand

let oasis_fn =
  ref "_oasis"

let main () =
  let _chngs : OASISFileTemplate.file_generate_change list = 
    BaseGenerate.generate 
      ~backup:false
      ~dev:false
      ~setup_fn:BaseSetup.default_fn
      ~restore:false
      (OASIS.from_file !oasis_fn)
  in
    ()

let scmd = 
  SubCommand.make 
    "setup" 
    (s_ "Translate _oasis into a build system")
    CLIData.setup_mkd
    main

let () = 
  SubCommand.register scmd
    
