
(** Like Setup but in development mode
    @author Sylvain Le Gall
  *)

open MainGettext
open SubCommand
open OASISFileTemplate

let roasis_exec =
  ref None

let main args =
  let oasis_exec = !roasis_exec in
  let setup_fn   = !Setup.rsetup_fn in

  let chngs = 
    BaseGenerate.generate 
      ~dev:true
      ~backup:true
      ~restore:false
      ?oasis_exec
      ~setup_fn
      (OASIS.from_file !Setup.oasis_fn)
  in
    (* Restore everything, except setup.ml *)
    List.iter 
      (function
         | Change (fn, bak) as chng ->
             if fn <> setup_fn then
               begin
                 file_rollback chng
               end
             else 
               begin
                 match bak with 
                   | Some fn -> FileUtil.rm [fn]
                   | None    -> ()
               end

         | Create fn as chng ->
             if fn <> setup_fn then
               file_rollback chng

         | NoChange ->
             ())
      chngs

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
             Arg.Unit (fun () -> roasis_exec := Some Sys.argv.(0)),
             (s_ " Use the real OASIS filename when generating developper mode \
                   setup.ml.");

           ]}

let () = 
  SubCommand.register scmd
