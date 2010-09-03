
(** Create the configure, build and install system 
    @author Sylvain Le Gall
  *)

open MainGettext
open OASISUtils
open SubCommand

let main () =
  let _chngs : OASISFileTemplate.file_generate_change list = 
    BaseGenerate.generate 
      ~backup:false
      ~dev:false
      ~setup_fn:BaseSetup.default_filename
      ~restore:false
      (OASISParse.from_file 
         ~ctxt:!BaseContext.default 
         !ArgCommon.oasis_fn)
  in
    ()

let scmd = 
  {(SubCommand.make 
      "setup" 
      (s_ "Translate _oasis into a build system")
      CLIData.setup_mkd
      main)
     with
         scmd_specs = ArgCommon.oasis_fn_specs}

let () = 
  SubCommand.register scmd
    
