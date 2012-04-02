(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2008-2010, OCamlCore SARL                                    *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)

(** META generator
    @author Sylvain Le Gall
  *)

open OASISGettext
open OASISTypes
open OASISValues
open OASISLibrary
open OASISFileTemplate
open OASISPlugin
open OASISSchema
open Format

type meta_type =
  | METALibrary
  | METASyntax

type t = 
    {
      enable:      bool;
      description: string option;
      meta_type:   meta_type;
      requires:    (string list) option;
      extra_lines: string list;
    }

let plugin = 
  `Extra, "META", Some OASISConf.version_short

let self_id, all_id = 
  Extra.create plugin

let pivot_data = 
  data_new_property plugin

let generator =
  let new_field nm = 
    new_field OASISLibrary.schema all_id nm
  in

  let enable = 
    new_field
      "Enable"
      ~default:true
      boolean
      (fun () ->
         s_ "Enable META generation")
      pivot_data (fun _ t -> t.enable)
  in

  let description =
    new_field
      "Description"
      ~default:None
      (opt string_not_empty)
      (fun () ->
         s_ "META package description")
      pivot_data (fun _ t -> t.description)
  in

  let meta_type =
    new_field
      "Type"
      ~default:METALibrary
      (choices
         (fun () ->
            s_ "META type")
         [
           "library", METALibrary;
           "syntax",  METASyntax;
         ])
      (fun () ->
         s_ "Type of META package, set default predicates for archive")
      pivot_data (fun _ t -> t.meta_type)
  in

  let extra_lines =
    new_field
      "ExtraLines"
      ~default:[]
      ~since_version:"0.3"
      (newline_separated string_not_empty)
      (fun () ->
         s_ "Extra lines to add to the META")
      pivot_data (fun _ t -> t.extra_lines)
  in

  let requires =
    new_field
      "Requires"
      ~default:None
      (opt (comma_separated string))
      (fun () ->
         s_ "Requires field for META package")
      pivot_data (fun _ t -> t.requires)
  in

    fun data ->
      {
        enable      = enable data;
        description = description data;
        meta_type   = meta_type data;
        requires    = requires data;
        extra_lines = extra_lines data;
      }

let pp_print_meta pkg root_t findlib_name_map fmt grp =

  let replace_chars s =
    BatString.replace_chars
      (function
         | '\n' | '\t' | '\r' -> " "
         | c -> String.make 1 c)
      s
  in

  let pp_print_field fmt (var, preds, vl) = 
    fprintf fmt
      "@,@[%s(%s) =@ %S@]"
      var 
      (String.concat ", " preds)
      (replace_chars vl)
  in
  let pp_print_sfield fmt (var, vl) = 
    fprintf fmt "@,@[%s =@ %S@]" var (replace_chars vl)
  in

  let default_synopsis = 
    match root_t.description with
      | Some txt -> txt
      | None -> pkg.synopsis
  in

  let rec pp_print_library fmt (lib_cs, lib_bs, lib, children) =
    let lib_name =
      lib_cs.cs_name
    in
    let lib_cma, lib_cmxa, lib_cmxs =
      lib_name^".cma", lib_name^".cmxa", lib_name^".cmxs"
    in
    let t = 
      generator lib_cs.cs_data
    in
      pp_print_sfield fmt ("version", (OASISVersion.string_of_version pkg.version));
      begin
        let txt =
          match t.description with
            | Some txt -> txt
            | None -> default_synopsis 
        in
          pp_print_sfield fmt ("description", txt)
      end;
      begin 
        let requires = 
          match t.requires with 
            | Some lst ->
                lst
            | None ->
                List.map 
                  (function
                     | InternalLibrary nm ->
                         OASISLibrary.findlib_of_name 
                           ~recurse:true
                           findlib_name_map 
                           nm
                     | FindlibPackage (nm, _) ->
                         nm)
                  lib_bs.bs_build_depends
        in
         if requires <> [] then
          pp_print_sfield fmt ("requires", String.concat " " requires)
      end; 
      begin
        match t.meta_type with 
          | METALibrary ->
              pp_print_field fmt ("archive", ["byte"], lib_cma);
              pp_print_field fmt ("archive", ["byte"; "plugin"], lib_cma);
              begin
                match lib_bs.bs_compiled_object with
                  | Best | Native ->
                      pp_print_field fmt ("archive", ["native"], lib_cmxa);
                      pp_print_field fmt ("archive", ["native"; "plugin"], 
                                          lib_cmxs)
                  | Byte ->
                      ()
              end

          | METASyntax ->
              pp_print_field fmt ("archive", ["syntax"; "preprocessor"], lib_cma);
              pp_print_field fmt ("archive", ["syntax"; "toploop"], lib_cma)
      end;
      List.iter (fprintf fmt "@,%s") t.extra_lines;
      pp_print_sfield fmt 
        ("exists_if", 
         if lib_bs.bs_compiled_object = Native then 
           lib_cmxa
         else
           lib_cma);
      FormatExt.pp_print_list pp_print_group "@," fmt children

  and pp_print_group fmt = 
    function 
      | Container (fndlb_nm, children) ->
          fprintf fmt 
            "@,@[<v1>@[package %S (@]%a%a@]@,)"
            fndlb_nm
            
            pp_print_sfield 
            ("description", "Virtual container")
            
            (FormatExt.pp_print_list pp_print_group "") children

      | Package (fndlb_nm, lib_cs, lib_bs, lib, children) ->
          let t = 
            generator lib_cs.cs_data
          in
            if t.enable then
              fprintf fmt "@,@[<v1>@[package %S (@]%a@]@,)"
                fndlb_nm
                pp_print_library (lib_cs, lib_bs, lib, children)
  in

    assert(root_t.enable);
    pp_open_vbox fmt 0;
    fprintf fmt "# OASIS_START";
    begin
      match grp with 
        | Container (_, children) ->
            FormatExt.pp_print_list pp_print_group "" fmt children
        | Package (_, lib_cs, lib_bs, lib, children) ->
            pp_print_library fmt (lib_cs, lib_bs, lib, children)
    end;
    fprintf fmt "@,# OASIS_STOP@,";
    pp_close_box fmt ();
    pp_print_flush fmt ()

let main ctxt pkg =
  let findlib_name_map = 
    findlib_name_map pkg
  in
  let meta_created = Hashtbl.create 3 in
    List.fold_left 
      (fun ctxt grp ->
         let root_cs, root_bs, root_lib = 
           root_of_group grp
         in
         let root_t =
           generator root_cs.cs_data
         in
           if root_t.enable then
             begin
               let meta_fn =
                 Filename.concat root_bs.bs_path "META"
               in
               let buff =
                 Buffer.create 13
               in
                 if Hashtbl.mem meta_created meta_fn then
                   OASISUtils.failwithf
                     (f_ "The file '%s' generated for the library '%s' is \
                          already used for the library '%s'. You can make \
                          one a child of the other to solve this \
                          (field `FindlibParent:`).")
                     meta_fn root_cs.cs_name
                     (Hashtbl.find meta_created meta_fn);
                 Hashtbl.add meta_created meta_fn root_cs.cs_name;

                 pp_print_meta
                   pkg
                   root_t
                   findlib_name_map
                   (Format.formatter_of_buffer buff)
                   grp;
                 OASISPlugin.add_file
                   (template_of_string_list
                      ~ctxt:ctxt.OASISPlugin.ctxt
                      ~template:true
                      meta_fn
                      comment_meta
                      (BatString.nsplit (Buffer.contents buff) "\n"))
                   ctxt
             end
           else
             ctxt)
      ctxt
      (group_libs pkg)


let init () =
  register_help
    plugin
    {(help_default METAData.readme_template_mkd) with
         help_order = 40};
  Extra.register_act self_id main;
  register_generator_section `Library all_id pivot_data generator
