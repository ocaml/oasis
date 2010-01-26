
(** Generate custom configure/build/doc/test/install system
    @author
  *)

open BaseEnv;;

let plugin_id = "Custom";;

let run_and_replace cmd args extra_args =
  BaseExec.run 
    (var_expand cmd)
    (List.map 
       var_expand
       (args @ (Array.to_list extra_args)))

let run_and_replace_test cmd args =
  try
    run_and_replace cmd args [||];
    0.0
  with Failure _ ->
    1.0

let run_and_replace_clean cmd args () = 
  run_and_replace cmd args [||]

(* END EXPORT *)

open CommonGettext;;
open BasePlugin;;
open ODN;;
open OASISTypes;;
open OASISValues;;
open PropList.FieldRO;;

let common cmd cmd_clean cmd_distclean =

  let setup_code =
    BaseExec.code_of_apply_command_line 
      "CustomPlugin.run_and_replace" 
      cmd
  in

  let clean_code, distclean_code =
    let code_clean_run =
      function 
        | Some cmd ->
            Some 
              (BaseExec.code_of_apply_command_line
                 "CustomPlugin.run_and_replace_clean"
                 cmd)
        | None ->
            None
    in
      code_clean_run cmd_clean,
      code_clean_run cmd_distclean
  in

    {
      moduls = 
        [
          CustomData.customsys_ml;
        ];
      setup_code      = setup_code;
      clean_code      = clean_code;
      distclean_code  = distclean_code;
      other_action    = ignore;
      files_generated = [];
    }
;;

let new_field nm hlp hlp_clean hlp_distclean =
  OASIS.new_field
    OASISPackage.schema
    plugin_id
    nm
    string_not_empty
    hlp,
  OASIS.new_field
    OASISPackage.schema
    plugin_id
    (nm^"Clean")
    ~default:None
    (opt string_not_empty)
    hlp_clean,
  OASIS.new_field
    OASISPackage.schema
    plugin_id
    (nm^"Distclean")
    ~default:None
    (opt string_not_empty)
    hlp_distclean
;;

let build = 
  new_field
    "Build"
    (fun () -> s_ "Run command to build.")
    (fun () -> s_ "Run command to clean build step.")
    (fun () -> s_ "Run command to distclean build step.")
;;

let doc =
  new_field 
    "Doc"
    (fun () -> s_ "Run command to build documentation.")
    (fun () -> s_ "Run command to clean build documentation step.")
    (fun () -> s_ "Run command to distclean build documentation step.")
;;

let install =
  new_field
    "Install"
    (fun () -> s_ "Run command to install.")
    (fun () -> s_ "Run command to clean install step.")
    (fun () -> s_ "Run command to distclean install step.")
;;

let uninstall =
  new_field
    "Uninstall"
    (fun () -> s_ "Run command to uninstall.")
    (fun () -> s_ "Run command to clean uninstall step.")
    (fun () -> s_ "Run command to distclean uninstall step.")
;;


let generic (fld, fld_clean, fld_distclean) pkg = 
    common
      (fld pkg.schema_data)
      (fld_clean pkg.schema_data)
      (fld_distclean pkg.schema_data),
    pkg
;;

let test_clean =
  OASIS.new_field
    OASISTest.schema
    plugin_id
    "Clean"
    ~default:None
    (opt string_not_empty)
    (fun () ->
       s_ "Run command to clean test step.")
;;

let test_distclean =
  OASIS.new_field
    OASISTest.schema
    plugin_id
    "Distclean"
    ~default:None
    (opt string_not_empty)
    (fun () ->
       s_ "Run command to distclean test step.")
;;

let test tst =
  {(common
      "false"
      (test_clean tst.OASISTypes.test_schema_data)
      (test_distclean tst.OASISTypes.test_schema_data))
     with 
         setup_code = APP ("CustomPlugin.run_and_replace_test", [], [])},
  tst
;;

let conf, conf_clean, conf_distclean = 
  new_field 
    "Conf"
    (fun () -> s_ "Run command to configure.")
    (fun () -> s_ "Run command to clean configure step.")
    (fun () -> s_ "Run command to distclean configure step.")
;;

let conf pkg = 
  common
    (conf pkg.schema_data)
    (conf_clean pkg.schema_data)
    (conf_distclean pkg.schema_data)
;;

List.iter 
  (plugin_register plugin_id)
  [
    Configure conf;
    Build     (generic build); 
    Doc       (generic doc); 
    Test      test; 
    Install   (generic install, generic uninstall);
  ]
;;
