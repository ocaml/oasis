
(** Like Setup but in development mode
    @author Sylvain Le Gall
  *)

open MainGettext
open SubCommand
open OASISFileTemplate
open BaseMessage

let roasis_exec =
  ref None

let run_args = 
  ref [] (* It is not possible to run anything with 0 args, we need
            at least an executable, so this is the default value
          *)

let main () =
  let oasis_exec = !roasis_exec in
  let setup_fn   = BaseSetup.default_filename in
  let msg = 
    {(!BaseContext.default) with OASISContext.verbose = false}
  in

  let clean_changes chngs = 
    (* Restore everything, except setup.ml *)
    List.iter 
      (function
         | Change (fn, bak) as chng ->
             if fn <> setup_fn then
               begin
                 file_rollback ~ctxt:msg chng
               end
             else 
               begin
                 match bak with 
                   | Some fn -> FileUtil.rm [fn]
                   | None    -> ()
               end

         | Create fn as chng ->
             if fn <> setup_fn then
               file_rollback ~ctxt:msg chng

         | NoChange ->
             ())
      chngs
  in

    match !run_args with 
      | [] ->
          begin
            (* Default mode: generate setup.ml *)
            let chngs = 
              BaseGenerate.generate 
                ~msg
                ~dev:true
                ~backup:true
                ~restore:false
                ?oasis_exec
                ~setup_fn
                (OASISParse.from_file 
                   ~ctxt:!BaseContext.default 
                   !ArgCommon.oasis_fn)
            in
              clean_changes chngs
          end

      | bootstrap_ocaml :: bootstrap_args ->
          begin
            let () = 
              (* Clean everything before running *)
              BaseGenerate.restore ~msg ()
            in

            (* Run mode: generate setup-dev.ml and run it *)
            let dev_fn = 
              let default =
                "setup-dev.ml"
              in
              let rec find_fn fn n =
                if n <= 100 then
                  begin
                    if Sys.file_exists fn then
                      find_fn
                        (Printf.sprintf "setup-dev-%02d.ml" n)
                        (n + 1)
                    else
                      fn
                  end
                else
                  OASISUtils.failwithf1
                    (f_ "File '%s' already exists, cannot generate it for \
                         dev-mode. Please remove it first.")
                    default
              in
                find_fn default 0
            in

            let chngs = 
              BaseGenerate.generate 
                ~msg
                ~dev:false
                ~backup:true
                ~restore:true
                ~setup_fn:dev_fn
                (OASISParse.from_file 
                   ~ctxt:!BaseContext.default
                   !ArgCommon.oasis_fn)
            in

            let safe_exit () = 
              clean_changes chngs
            in

            let bootstrap_args = 
              List.map
                (fun e ->
                   (* Replace setup.ml by setup-dev.ml *)
                   if e = BaseSetup.default_filename then
                     dev_fn
                   else
                     e)
                bootstrap_args 
            in

            let exit_code = 
              ref 0
            in

              try 
                begin
                  (* Run command after regeneration *)
                  BaseExec.run
                    ~f_exit_code:(fun i -> exit_code := i)
                    bootstrap_ocaml
                    bootstrap_args;

                  safe_exit ();

                  if !exit_code <> 0 then
                    (* TODO: don't use exit *)
                    exit !exit_code
                end

              with e ->
                begin
                  safe_exit ();
                  error "%s" (string_of_exception e)
                end
          end 


let scmd = 
  {(SubCommand.make
      ~std_usage:true
      "setup-dev"
      (s_ "Translate _oasis into a build system that auto-update")
      CLIData.setup_dev_mkd
      main)
     with 
         scmd_specs =
           ([
             "-real-oasis",
             Arg.Unit (fun () -> roasis_exec := Some Sys.argv.(0)),
             s_ " Use the real OASIS filename when generating developper mode \
                  setup.ml.";

             "-run",
             Arg.Tuple 
               [Arg.Rest (fun a -> run_args := a :: !run_args);
                Arg.Unit (fun () -> run_args := List.rev !run_args)],
             s_ " Run a command after generating files, this is the mode used \
                  by setup.ml in developper mode. Don't use it directly.";
           ] @ ArgCommon.oasis_fn_specs)}

let () = 
  SubCommand.register scmd
