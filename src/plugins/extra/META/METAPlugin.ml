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

(** META generator
    @author Sylvain Le Gall
  *)

open OASISGettext
open OASISTypes
open OASISValues
open OASISLibrary
open OASISFileTemplate
open Format

module PU = OASISPlugin.Extra.Make
              (struct
                 let name = "META"
                 let version = OASISConf.version
                 let help = METAData.readme_template_mkd
                 let help_extra_vars = []
                 let help_order = 40
               end)
open PU

let new_field nm = 
  new_field OASISLibrary.schema nm

let enable = 
  new_field
    "Enable"
    ~default:true
    boolean
    (fun () ->
       s_ "Enable META generation")

let description =
  new_field
    "Description"
    ~default:None
    (opt string_not_empty)
    (fun () ->
       s_ "META package description")

type meta_type =
  | METALibrary
  | METASyntax

let typ =
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

let requires =
  new_field
    "Requires"
    ~default:None
    (opt (comma_separated string))
    (fun () ->
       s_ "Requires field for META package")

let pp_print_meta pkg findlib_name_map fmt grp =

  let pp_print_field fmt (var, preds, vl) = 
    fprintf fmt
      "@,@[<hv1>%s(@[%a@])@ =@ %S@]" 
      var 
      (FormatExt.pp_print_list pp_print_string ",@,") preds
      vl
  in
  let pp_print_sfield fmt (var, vl) = 
    fprintf fmt "@,@[<hv 1>%s@ =@ %S@]" var vl
  in

  let root_cs, root_bs, root_lib =
    root_of_group grp
  in

  let default_synopsis = 
    match description root_cs.cs_data with
      | Some txt -> txt
      | None -> pkg.synopsis
  in

  let rec pp_print_library fmt (lib_cs, lib_bs, lib, children) =
    let lib_name =
      lib_cs.cs_name
    in
      pp_print_sfield fmt ("version", (OASISVersion.string_of_version pkg.version));
      begin
        let txt =
          match description lib_cs.cs_data with
            | Some txt -> txt
            | None -> default_synopsis 
        in
          pp_print_sfield fmt ("description", txt)
      end;
      begin 
        let requires = 
          match requires lib_cs.cs_data with 
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
        match typ lib_cs.cs_data with 
          | METALibrary ->
              pp_print_field fmt ("archive", ["byte"], lib_name^".cma");
              begin
                match lib_bs.bs_compiled_object with
                  | Best | Native ->
                      pp_print_field fmt ("archive", ["native"], lib_name^".cmxa");
                  | Byte ->
                      ()
              end

          | METASyntax ->
              pp_print_field fmt ("archive", ["syntax"; "preprocessor"], lib_name^".cma");
              pp_print_field fmt ("archive", ["syntax"; "toploop"], lib_name^".cma")
      end;
      FormatExt.pp_print_list pp_print_group "@," fmt children

  and pp_print_group fmt = 
    function 
      | Container (fndlb_nm, children) ->
          fprintf fmt 
            "@,@[<hv1>package %S (%a%a@]@,)"
            fndlb_nm
            
            pp_print_sfield 
            ("description", "Virtual container")
            
            (FormatExt.pp_print_list pp_print_group "") children

      | Package (fndlb_nm, lib_cs, lib_bs, lib, children) ->
          if enable lib_cs.cs_data then
            fprintf fmt "@,@[<hv1>package %S (%a@]@,)"
              fndlb_nm
              pp_print_library (lib_cs, lib_bs, lib, children)
  in

    assert(enable root_cs.cs_data);
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
    List.fold_left 
      (fun ctxt grp ->
         let root_cs, root_bs, root_lib = 
           root_of_group grp
         in
           if enable root_cs.cs_data then
             begin
               let meta_fn =
                 Filename.concat root_bs.bs_path "META"
               in
               let buff =
                 Buffer.create 13
               in
                 pp_print_meta 
                   pkg 
                   findlib_name_map 
                   (Format.formatter_of_buffer buff) 
                   grp;
                 OASISPlugin.add_file
                   (template_of_string_list
                      ~ctxt:ctxt.OASISPlugin.ctxt
                      ~template:true
                      meta_fn 
                      comment_meta 
                      (ExtString.String.nsplit
                         (Buffer.contents buff)
                         "\n"))
                   ctxt
             end
           else
             ctxt)
      ctxt
      (group_libs pkg)


let init () = 
  register main
