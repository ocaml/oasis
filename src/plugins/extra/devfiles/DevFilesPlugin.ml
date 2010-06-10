(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2008-2010, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

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
                 let help = DevFilesData.readme_template_mkd
                 let help_extra_vars = []
                 let help_order = 60
               end)

open PU

let all_targets =
  [
    "build"; 
    "doc";
    "test";
    "all";
    "install";
    "uninstall";
    "reinstall";
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
      let add_one_target ?(need_configure=true) ?(other_depends=[]) nm = 
        Printf.bprintf buff 
          "%s: %s\n\
           \t$(SETUP) -%s $(%sFLAGS)\n\n" 
          nm 
          (String.concat " "
             ((if need_configure then 
                 (fun l -> "setup.data" :: l)
               else 
                 (fun l -> l))
                other_depends))
          nm (String.uppercase nm) 
      in
        Buffer.add_string buff "\nSETUP = ocaml setup.ml\n\n";
        List.iter
          (function
             | "all" | "clean" | "distclean" as nm ->
                 add_one_target ~need_configure:false nm
             | "test" | "doc" as nm ->
                 add_one_target ~other_depends:["build"] nm
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

let init () =
  register main
