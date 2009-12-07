
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

  let build_gen, pkg = 
    (plugin_build pkg.build_type) pkg
  in

  let test_gens, pkg = 
    let test_gens = 
      List.fold_left
        (fun test_gens (nm, tst) ->
           let gen, pkg = 
             (plugin_test tst.test_type) tst
           in
             ((nm, tst, gen) :: test_gens))
        []
        pkg.tests
    in
      List.rev test_gens,
      {pkg with tests = List.rev_map (fun (nm, tst, _) -> nm, tst) test_gens}
  in

  let install_gen, pkg =
    (plugin_install pkg.install_type) pkg
  in

  let uninstall_gen, pkg =
    (plugin_uninstall pkg.install_type) pkg
  in

  let configure_gen =
    (* We call last configure, so that we can collect variables and changes to
     * package defined in other plugins
     *)
    (plugin_configure pkg.conf_type) pkg
  in

  let () = 
    (* Run extra plugin *)
    List.iter
      (fun nm -> plugin_extra nm pkg)
      pkg.plugins
  in

  let all_actions =
    List.flatten 
      [[
         configure_gen;
         build_gen;
       ];
       (List.map 
          (fun (_, _, gen) -> gen)
          test_gens);
       [
         install_gen;
         uninstall_gen;
       ]]
  in

  let clean_code =
    FUN 
      (["()"],
       (* Process clean code in reverse order *)
       (List.flatten (List.rev_map (fun act -> act.clean_code) all_actions)))
  in

  let distclean_code =
    FUN 
      (["()"],
       (* Process distclean code in reverse order *)
       (List.flatten (List.rev_map (fun act -> act.distclean_code) all_actions)))
  in

  let files_generated_code =
    LST
      (List.map 
         (fun f -> STR f)
         (List.flatten (List.map (fun act -> act.files_generated) all_actions)))
  in

  let doc_code = 
    LST []
  in

  let test_code =
    BaseTest.generate test_gens
  in

  let setup_t_code =
    REC
      ("BaseSetup",
       [
         "configure",       configure_gen.setup_code;
         "build",           build_gen.setup_code;
         "test",            test_code;
         "doc",             doc_code;
         "install",         install_gen.setup_code;
         "uninstall",       uninstall_gen.setup_code;
         "clean",           clean_code;
         "distclean",       distclean_code;
         "files_generated", files_generated_code;
        ])
  in

  let setup_fun =
    fprintf str_formatter
      "@[<hv2>let setup () =@ %a@,@];;"
      pp_ocaml_expr (APP ("BaseSetup.setup", [], [setup_t_code]));
    flush_str_formatter ()
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

