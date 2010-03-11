
(** Generate standard development files
    @author Sylvain Le Gall
  *)

open BaseFileGenerate
open OASISGettext
open OASISTypes

module PU = OASISPlugin.Extra.Make
              (struct 
                 let name = "DevFiles" 
                 let version = OASISConf.version 
               end)

open PU

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
  new_field
    OASISPackage.schema
    "MakefileNoTargets"
    ~default:[]
    (OASISValues.comma_separated
       (OASISValues.choices
          (fun _ -> s_ "target")
          (List.rev_map (fun s -> s, s) all_targets)))
    (fun () ->
       s_ "Targets to disable when generating Makefile")

let enable_makefile =
  new_field
    OASISPackage.schema
    "EnableMakefile"
    ~default:true
    OASISValues.boolean
    (fun () ->
       s_ "Generate Makefile")

let enable_configure =
  new_field
    OASISPackage.schema
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
          OASISUtils.set_string_of_list 
            (makefile_notargets pkg.schema_data)
        in
          List.filter
            (fun t -> not (OASISUtils.SetString.mem t excludes))
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
  register main
