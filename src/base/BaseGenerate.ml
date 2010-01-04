
(** Generate package files
    @author Sylvain Le Gall
  *)

open Format;;
open OASISTypes;;
open BaseUtils;;
open BaseFileGenerate;;
open BaseGenCode;;
open BasePlugin;;

(** Generate autobuild system 
  *)
let generate pkg = 

  let pkg, setup_t_code, all_actions =
    BaseSetup.code_of_oasis pkg
  in

  let () = 
    (* Run extra plugin *)
    List.iter
      (fun nm -> plugin_extra nm pkg)
      pkg.plugins
  in

  let moduls =
    let module SSet = Set.Make(String)
    in
    let moduls = 
      List.flatten
        (List.map (fun act -> act.moduls) all_actions)
    in
    let (rmoduls, _) =
      List.fold_left
        (fun ((moduls, moduls_seen) as acc) modul ->
           if SSet.mem modul moduls_seen then
             acc
           else
             (modul :: moduls, SSet.add modul moduls_seen))
        ([], SSet.empty)
        moduls
    in
      List.rev rmoduls
  in

  let setup_fun =
    fprintf str_formatter
      "@[<hv2>let setup () =@ %a@,@];;"
      pp_ocaml_expr (APP ("BaseSetup.setup", [], [setup_t_code]));
    flush_str_formatter ()
  in

    (* Generate setup.ml *)
    mlfile_generate
      "setup.ml"
      (Split
         (
           (* Header *)
           ["#!/usr/bin/ocamlrun ocaml"],
           (* Body *)
           (List.flatten 
              [
                moduls;
                [setup_fun]
              ]),
           (* Footer *)
           ["setup ();;"]
         )
      );
    Unix.chmod "setup.ml" 0o755;

    (* Generate other files *)
    List.iter
      (fun act -> act.other_action ())
      all_actions
;;

