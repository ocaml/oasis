
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

  let generators, pkg =
    List.fold_left
      (fun (generators, pkg) (setup_nm, fact) ->
         let act, pkg =
           fact pkg
         in
           ((setup_nm, act) :: generators), pkg)
      ([], pkg)
      [
        "build",   (plugin_build   pkg.build_type);
        "doc",     (plugin_doc     pkg.doc_type);
        "test",    (plugin_test    pkg.test_type);
        "install", (plugin_install pkg.install_type);
      ]
  in

  let standard_vars =
    let module SVSet = 
      Set.Make
        (struct 
           type t = standard_var 
           let compare = compare
         end)
    in
      SVSet.elements
        (List.fold_left
           (fun set e -> SVSet.add e set)
           SVSet.empty
           (List.flatten 
              (List.map 
                 (fun (_, act) -> act.standard_vars)
                 generators)))
  in

  let configure =
     (plugin_configure pkg.conf_type) pkg standard_vars
  in

  let () = 
    (* Run extra plugin *)
    List.iter
      (fun nm -> plugin_extra nm pkg)
      pkg.plugins
  in

  let all_actions =
    configure
    ::
    (List.map snd generators)
  in

  let clean_code =
    FUN 
      (["()"],
       (List.flatten (List.map (fun act -> act.clean_code) all_actions)))
  in

  let distclean_code =
    FUN 
      (["()"],
       (List.flatten (List.map (fun act -> act.distclean_code) all_actions)))
  in

  let files_generated_code =
    LST
      (List.map 
         (fun f -> STR f)
         (List.flatten (List.map (fun act -> act.files_generated) all_actions)))
  in

  let setup_t_code =
    REC
      ("BaseSetup",
       (
         (List.map (fun (nm, act) -> nm, act.setup_code) generators)
         @
         [
           "configure",       configure.setup_code;
           "clean",           clean_code;
           "distclean",       distclean_code;
           "files_generated", files_generated_code;
         ]
       )
      )
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
           [],
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

    (* Generate other files *)
    List.iter
      (fun act -> act.other_action ())
      all_actions
;;

