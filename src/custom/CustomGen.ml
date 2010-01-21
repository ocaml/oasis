
(** Generate custom configure/build/doc/test/install system
    @author
  *)

open CommonGettext;;
open BasePlugin;;
open BaseGenCode;;
open OASISTypes;;
open OASISValues;;
open PropList.FieldRO;;

let plugin_id = "Custom";;

let common cmd cmd_clean cmd_distclean =

  let code_replace_and_run = 
    BaseExec.code_of_apply_command_line 
      "CustomUtils.run_and_replace" 
  in

  let code_run =
    BaseExec.code_of_apply_command_line
      "BaseExec.run"
  in

  let setup_code =
    code_replace_and_run cmd
  in
  let clean_code =
    match cmd_clean with 
      | Some cmd ->
          [code_run cmd]
      | None ->
          []
  in
  let distclean_code =
    match cmd_distclean with 
      | Some cmd ->
          [code_run cmd]
      | None ->
          []
  in
    {
      moduls = 
        [
          CommonData.commonsys_ml;
          BaseData.basesys_ml;
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
         setup_code = APP ("CustomUtils.run_and_replace_test", [], [])},
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
