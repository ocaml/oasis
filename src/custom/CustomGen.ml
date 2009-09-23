
(** Generate custom configure/build/doc/test/install system
    @author
  *)

open CommonGettext;;
open BaseGenerate;;
open BaseGenCode;;
open OASISTypes;;
open OASISValueParser;;

let plugin_id = 
  "custom"
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

let conf = 
  new_field 
    "Conf"
    (s_ "Run command to configure.")
    (s_ "Run command to clean configure step.")
    (s_ "Run command to distclean configure step.")
;;

let build = 
  new_field
    "Build"
    (s_ "Run command to build.")
    (s_ "Run command to clean build step.")
    (s_ "Run command to distclean build step.")
;;

let doc =
  new_field 
    "Doc"
    (s_ "Run command to build documentation.")
    (s_ "Run command to clean build documentation step.")
    (s_ "Run command to distclean build documentation step.")
;;

let test =
  new_field
    "Test"
    (s_ "Run command to test.")
    (s_ "Run command to clean test step.")
    (s_ "Run command to distclean test step.")
;;

let install =
  new_field
    "Install"
    (s_ "Run command to install.")
    (s_ "Run command to clean install step.")
    (s_ "Run command to distclean install step.")
;;

let main (fld, fld_clean, fld_distclean) pkg = 
  let code func str =
    let cmd, args =
      match BaseUtils.split ' ' str with
        | cmd :: args ->
            STR cmd, LST (List.map (fun s -> STR s) args)
        | [] ->
            failwith (s_ "Empty custom command")
    in
      APP (func,
           [],
           [cmd; args])
  in

  let code_replace_and_run = 
    code "CustomUtils.run_and_replace"
  in

  let code_run =
    code "BaseExec.run"
  in

  let setup_code =      
    code_replace_and_run (fld pkg.schema_data)
  in
  let clean_code =
    match fld_clean pkg.schema_data with 
      | Some cmd ->
          [code_run cmd]
      | None ->
          []
  in
  let distclean_code =
    match fld_distclean pkg.schema_data with 
      | Some cmd ->
          [code_run cmd]
      | None ->
          []
  in
    {
      moduls = 
        [
          BaseData.basesys_ml;
          CustomData.customsys_ml;
        ];
      setup_code      = setup_code;
      clean_code      = clean_code;
      distclean_code  = distclean_code;
      other_action    = ignore;
      files_generated = [];
      standard_vars   = [];
    },
    pkg
;;

List.iter 
 (fun (knd, fld) ->
   generator_register
     knd
     plugin_id
     (main fld))
  [
    Build,   build; 
    Doc,     doc; 
    Test,    test; 
    Install, install;
  ]
;;

configure_generator_register
  plugin_id
  (fun pkg standard_vars -> fst (main conf pkg))
;;
