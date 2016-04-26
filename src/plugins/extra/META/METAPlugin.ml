(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2013, Sylvain Le Gall                                   *)
(* Copyright (C) 2008-2011, OCamlCore SARL                                    *)
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
open OASISFindlib
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


let feature_extra_lines =
  OASISFeatures.create "extra_lines" ~plugin
    (OASISFeatures.since_version "0.3")
    (fun () ->
       s_ "Allow to add extra lines to the generated META.")


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
      ~feature:feature_extra_lines
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


let pp_print_meta pkg root_t findlib_name_of_library_name fmt grp =
  let may f =
    function
      | Some x -> f x
      | None -> ()
  in

  let replace_chars s =
    OASISString.replace_chars
      (function
        | '\n' | '\t' | '\r' -> ' '
        | c -> c)
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

  let rec pp_print_library fmt (cs, bs, contents, children) =
    let name = cs.cs_name in
    let archive_byte, archive_byte_plugin,
      archive_native, archive_native_plugin =
      match contents with
        | `Library _ ->
          name^".cma", Some (name^".cma"),
          name^".cmxa", Some (name^".cmxs")
        | `Object { obj_modules = [ m ] } ->
          let dir = OASISHostPath.of_unix bs.bs_path in
          let exists fn ext =
            let path = Filename.concat dir (fn ^ ext) in
            OASISFileUtil.file_exists_case path
          in
          let m =
            prerr_endline (Printf.sprintf "module: %s" m);
            List.find
              (fun m -> exists m ".mli" || exists m ".ml")
              [ String.uncapitalize m ; String.capitalize m ]
          in
          m^".cmo", None,
          m^".cmx", None
        | `Object _ ->
          name^".cmo", None,
          name^".cmx", None

    in
    let t =
      generator cs.cs_data
    in
    pp_print_sfield fmt
      ("version", (OASISVersion.string_of_version pkg.version));
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
                  findlib_name_of_library_name nm
                | FindlibPackage (fndlb_nm, _) ->
                  fndlb_nm)
              bs.bs_build_depends
      in
      if requires <> [] then
        pp_print_sfield fmt ("requires", String.concat " " requires)
    end;
    begin
      match t.meta_type with
        | METALibrary ->
          pp_print_field fmt ("archive", ["byte"], archive_byte);
          may
            (fun x ->
               pp_print_field fmt ("archive", ["byte"; "plugin"], x))
            archive_byte_plugin;
          begin
            match bs.bs_compiled_object with
              | Best | Native ->
                pp_print_field fmt ("archive", ["native"], archive_native);
                may
                  (fun x ->
                     pp_print_field fmt
                       ("archive", ["native"; "plugin"], x))
                  archive_native_plugin;
              | Byte ->
                ()
          end

        | METASyntax ->
          pp_print_field fmt
            ("archive", ["syntax"; "preprocessor"], archive_byte);
          pp_print_field fmt
            ("archive", ["syntax"; "toploop"], archive_byte);
          begin match bs.bs_compiled_object with
            | Best | Native ->
              pp_print_field fmt
                ("archive", ["syntax"; "preprocessor"; "native"], archive_native);
              may
                (fun x ->
                   pp_print_field fmt
                     ("archive", ["syntax"; "preprocessor"; "native"; "plugin"], x))
                archive_native_plugin;
            | Byte ->
              ()
          end
    end;
    List.iter (fprintf fmt "@,%s") t.extra_lines;
    pp_print_sfield fmt
      ("exists_if",
       if bs.bs_compiled_object = Native then
         archive_native
       else
         archive_byte);
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
  let group_libs, findlib_name_of_library_name, _ =
    findlib_mapping pkg
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
             OASISUnixPath.concat root_bs.bs_path "META"
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
             findlib_name_of_library_name
             (Format.formatter_of_buffer buff)
             grp;
           OASISPlugin.add_file
             (template_of_string_list
                ~ctxt:ctxt.OASISPlugin.ctxt
                ~template:true
                meta_fn
                comment_meta
                (OASISString.split_newline ~do_trim:false
                   (Buffer.contents buff)))
             ctxt
         end
       else
         ctxt)
    ctxt
    group_libs


let init () =
  register_help
    plugin
    {(help_default METAData.readme_template_mkd) with
       help_order = 40};
  Extra.register_act self_id main;
  register_generator_section `Library all_id pivot_data generator
