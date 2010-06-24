
(** Create the configure, build and install system 
    @author Sylvain Le Gall
  *)

open MainGettext
open OASISUtils
open SubCommand

let oasis_fn =
  ref "_oasis"

let rsetup_fn =
  ref BaseSetup.default_fn

let main args =
  BaseGenerate.generate 
    (OASIS.from_file !oasis_fn)
    false
    !rsetup_fn
    false

let scmd = 
  {(SubCommand.make 
      ~std_usage:true
      "setup" 
      (s_ "Translate _oasis into a build system")
      CLIData.setup_mkd
      main)
     with
         scmd_specs =
           [
             "-setup-fn",
             Arg.Set_string rsetup_fn,
             (s_ "fn Change the default name of setup.ml. This option should \
                     be used with caution, it is reserved for internal use.");
           ];
         scmd_main = main}

let () = 
  SubCommand.register scmd
    
