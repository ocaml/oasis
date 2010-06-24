
(** Like Setup but in development mode
    @author Sylvain Le Gall
  *)

open MainGettext
open SubCommand

let ruse_oasis_real_filename =
  ref false

let main args =
  BaseGenerate.generate 
    (OASIS.from_file !Setup.oasis_fn)
    true
    !Setup.rsetup_fn
    !ruse_oasis_real_filename

let scmd = 
  {(SubCommand.make
      ~std_usage:true
      "setup-dev"
      (s_ "Translate _oasis into a build system that auto-update")
      CLIData.setup_dev_mkd
      main)
     with 
         scmd_specs =
           [
             "-real-oasis",
             Arg.Set ruse_oasis_real_filename,
             (s_ " Use the real OASIS filename when generating developper mode \
                   setup.ml.");
           ]}

let () = 
  SubCommand.register scmd
