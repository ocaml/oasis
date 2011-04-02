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

open OASISFileTemplate
open OASISGettext
open OASISTypes
open OASISPlugin
open OASISSchema

let plugin = `Extra, "DevFiles", Some OASISConf.version_short

let self_id, all_id = 
  Extra.create plugin

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

type t = 
    {
      makefile_notargets: string list;
      enable_makefile:    bool;
      enable_configure:   bool;
    }

let pivot_data = 
  data_new_property plugin

let generator = 
  let makefile_notargets =
    new_field
      OASISPackage.schema
      all_id
      "MakefileNoTargets"
      ~default:[]
      (OASISValues.comma_separated
         (OASISValues.choices
            (fun _ -> s_ "target")
            (List.rev_map (fun s -> s, s) all_targets)))
      (fun () ->
         s_ "Targets to disable when generating Makefile")
      pivot_data (fun _ t -> t.makefile_notargets)
  in

  let enable_makefile =
    new_field
      OASISPackage.schema
      all_id
      "EnableMakefile"
      ~default:true
      OASISValues.boolean
      (fun () ->
         s_ "Generate Makefile")
      pivot_data (fun _ t -> t.enable_makefile)
  in

  let enable_configure =
    new_field
      OASISPackage.schema
      all_id
      "EnableConfigure"
      ~default:true
      OASISValues.boolean
      (fun () ->
         s_ "Generate configure script")
      pivot_data (fun _ t -> t.enable_configure)
  in

    (fun data ->
       {
         makefile_notargets = makefile_notargets data;
         enable_makefile    = enable_makefile data;
         enable_configure   = enable_configure data;
       })


let main ctxt pkg = 
  let t = 
    generator pkg.schema_data
  in
  let ctxt = 
    (* Generate Makefile (for standard dev. env.) *)
    if t.enable_makefile then
      begin
        let buff = 
          Buffer.create 13
        in
        let targets =
          let excludes =
            OASISUtils.set_string_of_list t.makefile_notargets
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
            targets;
          Buffer.add_string buff (".PHONY: "^(String.concat " " targets)^"\n");
    
          OASISPlugin.add_file 
            (template_make 
               "Makefile"
               comment_sh
               []
               (ExtString.String.nsplit (Buffer.contents buff) "\n")
               [])
            ctxt
      end
    else
      ctxt
  in
  
  let ctxt = 
    (* Generate configure (for standard dev. env.) *)
    if t.enable_configure then
      begin
        let tmpl = 
          template_of_string_list  
            ~ctxt:ctxt.OASISPlugin.ctxt
            ~template:true
            "configure"
            comment_sh
            DevFilesData.configure
        in
          OASISPlugin.add_file
            {tmpl with perm = 0o755}
            ctxt
      end
    else
      ctxt
  in

    ctxt


let init () =
  Extra.register_act self_id main;
  register_help
    plugin
    {(help_default DevFilesData.readme_template_mkd) with 
         help_order = 60};
  register_generator_package all_id pivot_data generator
