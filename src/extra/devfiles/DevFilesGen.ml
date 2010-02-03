
(** Generate standard development files
    @author Sylvain Le Gall
  *)

open BasePlugin
open BaseFileGenerate
open CommonGettext
open OASISTypes

let plugin_id = "DevFiles"

let all_targets =
  [
    "build"; 
    "doc";
    "test";
    "install";
    "uninstall";
    "clean";
    "distclean";
    "configure";
  ]

let makefile_notargets =
  OASIS.new_field
    OASISPackage.schema
    plugin_id
    "MakefileNoTargets"
    ~default:[]
    (OASISValues.comma_separated
       (OASISValues.choices
          (fun _ -> s_ "target")
          (List.rev_map (fun s -> s, s) all_targets)))
    (fun () ->
       s_ "Targets to disable when generating Makefile")

let enable_makefile =
  OASIS.new_field
    OASISPackage.schema
    plugin_id
    "EnableMakefile"
    ~default:true
    OASISValues.boolean
    (fun () ->
       s_ "Generate Makefile")

let enable_configure =
  OASIS.new_field
    OASISPackage.schema
    plugin_id
    "EnableConfigure"
    ~default:true
    OASISValues.boolean
    (fun () ->
       s_ "Generate configure script")

let main pkg = 
  (* Generate Makefile (for standard dev. env.) *)
  if enable_makefile pkg.schema_data then
    begin
      let buff = 
        Buffer.create 13
      in
      let targets =
        let excludes =
          BaseUtils.set_string_of_list 
            (makefile_notargets pkg.schema_data)
        in
          List.filter
            (fun t -> not (BaseUtils.SetString.mem t excludes))
            all_targets
      in
      let add_one_target ?(need_configure=true) nm = 
        Printf.bprintf buff 
          "%s: %s\n\
           \t$(SETUP) -%s $(%sFLAGS)\n\n" 
          nm 
          (if need_configure then "setup.data" else "")
          nm (String.uppercase nm) 
      in
        Buffer.add_string buff "\nSETUP = ocaml setup.ml\n\n";
        List.iter
          (function
             | "clean" | "distclean" as nm ->
                 add_one_target ~need_configure:false nm
             | "configure" ->
                 Printf.bprintf buff 
                   "setup.data:\n\
                    \t$(SETUP) -configure $(CONFIGUREFLAGS)\n\n" 
             | nm ->
                 add_one_target nm)
          all_targets;
        Buffer.add_string buff (".PHONY: "^(String.concat " " targets)^"\n");
  
        file_generate
          "Makefile"
          comment_sh
          (Split
             ([],
              ExtString.String.nsplit (Buffer.contents buff) "\n",
              []))
    end;

  (* Generate configure (for standard dev. env.) *)
  if enable_configure pkg.schema_data then
    begin
      file_generate
        "configure"
        comment_sh
        (NeedSplit
           DevFilesData.configure);
      Unix.chmod "configure" 0o755
    end

let () =
  plugin_register 
    plugin_id 
    (Extra main)
