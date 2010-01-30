
(** Generate custom configure/build/doc/test/install system
    @author
  *)

open BaseEnv

TYPE_CONV_PATH "CustomPlugin"

type t =
    {
      cmd_main:     string * (string list);
      cmd_clean:     (string * (string list)) option;
      cmd_distclean: (string * (string list)) option;
    } with odn

let run cmd args extra_args =
  BaseExec.run 
    (var_expand cmd)
    (List.map 
       var_expand
       (args @ (Array.to_list extra_args)))

let main {cmd_main = (cmd, args)} _ extra_args =
  run cmd args extra_args 

let clean t pkg extra_args =
  match t with
    | {cmd_clean = Some (cmd, args)} ->
        run cmd args extra_args
    | _ ->
        ()

let distclean t pkg extra_args =
  match t with
    | {cmd_distclean = Some (cmd, args)} ->
        run cmd args extra_args
    | _ ->
        ()

module Test =
struct
  let main t pkg nm test extra_args =
    try
      main t pkg extra_args;
      0.0
    with Failure _ ->
      1.0

  let clean t pkg nm test extra_args =
    clean t pkg extra_args

  let distclean t pkg nm test extra_args =
    distclean t pkg extra_args 
end

module Doc =
struct
  let main t pkg nm () extra_args =
    main t pkg extra_args

  let clean t pkg nm () extra_args =
    clean t pkg extra_args

  let distclean t pkg nm () extra_args =
    distclean t pkg extra_args
end

(* END EXPORT *)

open CommonGettext
open BasePlugin
open ODN
open OASISTypes
open OASISValues
open PropList.FieldRO

let plugin_id = "Custom"

(** Add standard fields 
  *)
let add_fields
      ?(schema=OASISPackage.schema)
      nm 
      hlp 
      hlp_clean 
      hlp_distclean =
  let cmd_main =
    OASIS.new_field
      schema
      plugin_id
      nm
      command_line
      hlp
  in
  let cmd_clean =
    OASIS.new_field
      schema
      plugin_id
      (nm^"Clean")
      ~default:None
      (opt command_line)
      hlp_clean
  in
  let cmd_distclean =
    OASIS.new_field
      schema
      plugin_id
      (nm^"Distclean")
      ~default:None
      (opt command_line)
      hlp_distclean
  in
    cmd_main, cmd_clean, cmd_distclean

(** Standard custom handling
  *)
let std nm hlp hlp_clean hlp_distclean =
  let cmd_main, cmd_clean, cmd_distclean =
    add_fields nm hlp hlp_clean hlp_distclean 
  in
    fun pkg -> 
      let t =
        {
          cmd_main      = cmd_main pkg.schema_data;
          cmd_clean     = cmd_clean pkg.schema_data;
          cmd_distclean = cmd_distclean pkg.schema_data;
        }
      in
        {
          moduls       = [CustomData.customsys_ml];
          setup        = func_with_arg 
                           main ("CustomPlugin.main")
                           t odn_of_t;
          clean        = Some 
                           (func_with_arg
                              clean ("CustomPlugin.clean")
                              t odn_of_t);
          distclean    = Some 
                           (func_with_arg
                              distclean ("CustomPlugin.distclean")
                              t odn_of_t);
          other_action = ignore;
        },
        pkg

let build = 
  std
    "Build"
    (fun () -> s_ "Run command to build.")
    (fun () -> s_ "Run command to clean build step.")
    (fun () -> s_ "Run command to distclean build step.")

let install =
  std
    "Install"
    (fun () -> s_ "Run command to install.")
    (fun () -> s_ "Run command to clean install step.")
    (fun () -> s_ "Run command to distclean install step.")

let uninstall =
  std
    "Uninstall"
    (fun () -> s_ "Run command to uninstall.")
    (fun () -> s_ "Run command to clean uninstall step.")
    (fun () -> s_ "Run command to distclean uninstall step.")

let conf = 
  std
    "Conf"
    (fun () -> s_ "Run command to configure.")
    (fun () -> s_ "Run command to clean configure step.")
    (fun () -> s_ "Run command to distclean configure step.")

let doc =
  let cmd_main, cmd_clean, cmd_distclean =
    add_fields
      (* TODO: use document *)
      ~schema:OASISPackage.schema
      "Doc"
      (fun () -> s_ "Run command to build documentation.")
      (fun () -> s_ "Run command to clean build documentation step.")
      (fun () -> s_ "Run command to distclean build documentation step.")
  in
    fun pkg nm doc ->
      let t =
        {
          cmd_main      = cmd_main pkg.schema_data;
          cmd_clean     = cmd_clean pkg.schema_data;
          cmd_distclean = cmd_distclean pkg.schema_data;
        }
      in
        {
          moduls       = [CustomData.customsys_ml];
          setup        = func_with_arg 
                           Doc.main ("CustomPlugin.Doc.main")
                           t odn_of_t;
          clean        = Some 
                           (func_with_arg
                              Doc.clean ("CustomPlugin.Doc.clean")
                              t odn_of_t);
          distclean    = Some 
                           (func_with_arg
                              Doc.distclean ("CustomPlugin.Doc.distclean")
                              t odn_of_t);
          other_action = ignore;
        },
        pkg,
        doc

let test =
  let test_clean =
    OASIS.new_field
      OASISTest.schema
      plugin_id
      "Clean"
      ~default:None
      (opt command_line)
      (fun () ->
         s_ "Run command to clean test step.")
  in
  let test_distclean =
    OASIS.new_field
      OASISTest.schema
      plugin_id
      "Distclean"
      ~default:None
      (opt command_line)
      (fun () ->
         s_ "Run command to distclean test step.")
  in
    fun pkg nm test -> 
      let t = 
        { 
          cmd_main      = test.test_command;
          cmd_clean     = test_clean test.test_schema_data;
          cmd_distclean = test_distclean test.test_schema_data;
        }
      in
        {
          moduls       = [CustomData.customsys_ml];
          setup        = func_with_arg 
                           Test.main ("CustomPlugin.Test.main")
                           t odn_of_t;
          clean        = Some 
                           (func_with_arg
                              Test.clean ("CustomPlugin.Test.clean")
                              t odn_of_t);
          distclean    = Some 
                           (func_with_arg
                              Test.distclean ("CustomPlugin.Test.distclean")
                              t odn_of_t);
          other_action = ignore;
        },
        pkg,
        test

let () =
  List.iter 
    (plugin_register plugin_id)
    [
      Configure conf;
      Build     build; 
      Doc       doc;
      Test      test;
      Install   (install, uninstall);
    ]
