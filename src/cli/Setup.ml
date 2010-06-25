
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

let rbackup =
  ref false

let main () =
  let _chngs : OASISFileTemplate.file_generate_change list = 
    BaseGenerate.generate 
      ~backup:!rbackup
      ~dev:false
      ~setup_fn:!rsetup_fn
      ~restore:false
      (OASIS.from_file !oasis_fn)
  in
    ()

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

             "-backup",
             Arg.Set rbackup,
             (s_ " Backup generated files and register them in the log \
                   file.")
           ];
         scmd_main = main}

let () = 
  SubCommand.register scmd
    
