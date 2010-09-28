
(** Run a oasis writer helper
    @author Sylvain Le Gall
  *)

open MainGettext
open OASISQuickstart
open OASISTypes
open SubCommand

let qckstrt_lvl =
  ref Beginner

let interf = 
  ref Human

let main () =
  OASISQuickstart.to_file 
    ~ctxt:!BaseContext.default
    !ArgCommon.oasis_fn
    !qckstrt_lvl
    !interf
    SetupDev.main

let scmd =
  let lvls =
    [
      s_ "beginner", Beginner; 
      s_ "intermediate", Intermediate; 
      s_ "expert", Expert;
    ]
  in
    {(SubCommand.make
        ~std_usage:true
        "quickstart"
        (s_ "Launch an helper to write `_oasis` file")
        CLIData.quickstart_mkd
        main)
       with 
           scmd_specs =
             (
               "-level",
               Arg.Symbol
                 ((List.map fst lvls),
                  (fun s -> qckstrt_lvl := List.assoc s lvls)),
               (s_ " Quickstart level, skip questions according to this level.")
             ) 
             ::
             (
               "-machine",
               Arg.Unit (fun () -> interf := Machine), 
               (s_ " Computer readable questions for automatic completion.")
             )
             :: SetupDev.scmd.scmd_specs}

let () = 
  SubCommand.register scmd
