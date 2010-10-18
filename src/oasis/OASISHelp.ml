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

(* TODO: move this file to src/cli/Manual.ml *)

(** Display help for OASIS
    @author Sylvain Le Gall
  *)

open OASISLicense
open OASISTypes
open OASISSchema_intern
open OASISGettext
open OASISUtils
open Format
open FormatExt

let fields_of_section ?plugin schm =
  List.rev
    (PropList.Schema.fold
       (fun acc key extra help ->
          match extra.kind, plugin with
            | StandardField, None 
            | DefinePlugin _, None
            | DefinePlugins _, None ->
                (key, help) :: acc
            | FieldFromPlugin plg, Some plg'  ->
                if OASISPlugin.plugin_compare plg' plg = 0 then
                  (key, help) :: acc
                else
                  acc
            | FieldFromPlugin _, None 
            | _, Some _ ->
                acc)
       []
       schm)

let pp_section_fields ?plugin ?allowed_fields schm = 

  let schm = 
    schm.schm
  in

  let fields = 
    let all_fields =
      fields_of_section ?plugin schm
    in
      match allowed_fields with
        | Some st ->
            List.filter 
              (fun (f, _) -> SetString.mem f st)
              all_fields
        | None ->
            all_fields
  in
          
  let fake_data =
    PropList.Data.create ()
  in

  let fmt =
    str_formatter
  in
    pp_set_margin fmt 80;
    pp_open_vbox fmt 0;
    pp_print_list
      (fun fmt (key, help) ->
         let help = 
           (match help with
              | Some h -> h ()
              | None -> "<No help>")^
           (
             try 
               let _s : string = 
                 PropList.Schema.get schm fake_data key
               in
                 ""
             with 
               | PropList.Not_set _ ->
                   s_ " (__mandatory__)"
               | PropList.No_printer _ | OASISValues.Not_printable ->
                   ""
           )
         in
           fprintf fmt 
             (f_ " * @[`%s`: %a@]") 
             key
             pp_print_string_spaced help)
        "@,"
        fmt
        fields;
    pp_close_box fmt ();
    flush_str_formatter ()


let pp_short_licenses () = 
  let fmt =
    str_formatter
  in
    pp_set_margin fmt 80;
    pp_open_vbox fmt 0;
    pp_print_list
      (fun fmt (license, data) ->
         let str_license = 
            string_of_license license
         in
         let long_name fmt = 
            pp_print_string_spaced fmt data.long_name
         in
         let vers = 
           List.map OASISVersion.string_of_version data.versions
         in
         match vers, data.note with
           | [], None ->
               fprintf fmt 
                 (f_ " * @[`%s`: %t@]") 
                 str_license long_name
           | [], Some txt ->
               fprintf fmt 
                 (f_ " * @[`%s`: %t. %a@]") 
                 str_license long_name
                 pp_print_string_spaced txt
           | lst, None ->
               fprintf fmt
                 (fn_ 
                    " * @[`%s`: %t (version@ %a)@]"
                    " * @[`%s`: %t (versions@ %a)@]"
                    (List.length vers))
                 str_license long_name
                 (pp_print_list pp_print_string ",@, ") lst
           | lst, Some txt ->
               fprintf fmt
                 (fn_ 
                    " * @[`%s`: %t. %a (version@ %a)@]"
                    " * @[`%s`: %t. %a (versions@ %a)@]"
                    (List.length vers))
                 str_license long_name
                 pp_print_string_spaced txt
                 (pp_print_list pp_print_string ",@, ") lst) 
        "@,"
        fmt
        (OASISLicense.license_data ());
    pp_close_box fmt ();
    flush_str_formatter ()

let pp_license_exceptions () = 
  let fmt =
    str_formatter
  in
    pp_set_margin fmt 80;
    pp_open_vbox fmt 0;
    pp_print_list
      (fun fmt (excpt, data) ->
         let excpt_str = 
          string_of_license_exception excpt
         in
         let explanation fmt = 
           pp_print_string_spaced fmt data.explanation
         in
         let licenses = 
           List.map string_of_license data.licenses
         in
           match licenses with
             | [] ->
                 fprintf fmt 
                   (f_ " * @[`%s`: %t@]") 
                   excpt_str explanation
             | lst ->
                 fprintf fmt
                   (fn_ 
                      " * @[`%s` compatible with %a: %t@]"
                      " * @[`%s` compatible with %a: %t@]"
                      (List.length licenses))
                   excpt_str
                   (pp_print_list pp_print_string ",@, ") lst
                   explanation)
        "@,"
        fmt
        (OASISLicense.license_exception_data ());
    pp_close_box fmt ();
    flush_str_formatter ()

let pp_standard_variables display schm = 
  let env =
    PropList.Data.create ()
  in
  let fmt =
    str_formatter
  in
  let vars = 
    List.rev
      (PropList.Schema.fold
         (fun acc name def short_descr_opt ->
            if display name def then
              (name,
               (match short_descr_opt with
                  | Some txt -> Some (txt ())
                  | None -> None),
               (try 
                  Some (PropList.Schema.get schm env name)
                with PropList.Not_set _ -> 
                  None))
              :: acc
            else 
              acc)
         []
         schm)
  in
    pp_set_margin fmt 80;
    pp_open_vbox fmt 0;
    pp_print_list
      (fun fmt ->
         function 
           | name, None, _ ->
               fprintf fmt (f_ " * `%s`") name
           | name, Some descr, _ ->
               fprintf fmt (f_ " * @[`%s`: %a@]")
                 name
                 pp_print_string_spaced descr)
        "@,"
        fmt
        vars;
    pp_close_box fmt ();
    flush_str_formatter ()

let kind_str knd = 
    match knd with 
      | `Configure -> "conf"
      | `Build     -> "build"
      | `Doc       -> "doc"
      | `Test      -> "test"
      | `Install   -> "install"
      | `Extra     -> "extra"

(** Standard variables to replace in help files *)
let vars ?plugin () =
  let bn = 
    match plugin with 
      | None ->
          "OASIS"
      | Some (knd, nm, ver) ->
          (String.capitalize nm)^
          (String.capitalize (kind_str knd))
  in
    List.map 
      (fun (pre, suf, pp) ->
         pre^bn^suf, pp)
      [
        "List", "Tests",
        (fun () ->
           String.concat 
             "\n"
             (List.map
                (fun et ->
                   Printf.sprintf "* `%s(X)`" 
                     (OASISExpr.string_of_test et))
                OASISExpr.tests));

        "List", "PackageFields",
        (fun () -> pp_section_fields ?plugin OASISPackage.schema);

        "List", "FlagFields",
        (fun () -> pp_section_fields ?plugin OASISFlag.schema);

        "List", "BuildFields",
        (fun () -> pp_section_fields ?plugin OASISLibrary.schema);

        "List", "LibraryFields",
        (fun () -> pp_section_fields ?plugin OASISLibrary.schema);
        
        "List", "ExecutableFields",
        (fun () -> pp_section_fields ?plugin OASISExecutable.schema);

        "List", "DocumentFields",
        (fun () -> pp_section_fields ?plugin OASISDocument.schema);

        "List", "TestFields",
        (fun () -> pp_section_fields ?plugin OASISTest.schema);

        "List", "SourceRepositoryFields",
        (fun () -> pp_section_fields ?plugin OASISSourceRepository.schema);
      ]

let pp_help_replace ?loc vars fmt str = 
  let buff = 
    Buffer.create 13
  in
    List.iter 
      (fun str ->
         (* Replace variables *)
         Buffer.add_substitute 
           buff
           (fun nm -> 
              try 
                (List.assoc nm vars) ()
              with Not_found ->
                begin
                  match loc with 
                    | None -> 
                        failwithf2
                          (f_ "Unknown variable %s in documentation (line is '%s')")
                          nm str
                    | Some loc ->
                        failwithf3
                          (f_ "Unknown variable %s in documentation (line is '%s' in %s)")
                          nm str loc
                end)
           str;
         pp_print_string fmt (Buffer.contents buff);
         pp_print_newline fmt ();
         Buffer.clear buff)
      str

let pp_print_help ?plugin fmt pp_print_cli_help env_schm env_display =
  let build_section_fields, library_fields, executable_fields =
    let set_fields_of_section schm = 
      set_string_of_list 
        (List.rev_map fst 
           (fields_of_section ?plugin schm.schm))
    in
    let lib_flds = 
      set_fields_of_section OASISLibrary.schema
    in
    let exec_flds =
      set_fields_of_section OASISExecutable.schema
    in
    let common_flds =
      SetString.inter lib_flds exec_flds
    in
      common_flds,
      SetString.diff lib_flds common_flds,
      SetString.diff exec_flds common_flds
  in

  let pp_plugin fmt (nm, knds, vo, hlp) = 

    (* Create additional variables that match the different 
     * plugin kind.
     *)
    let mk_derived_vars knd = 
      let plugin = 
        knd, nm, vo
      in
        vars ~plugin ()
    in
    let extra_vars = 
      List.flatten (List.rev_map mk_derived_vars knds)
    in

    let all_kinds =
      String.concat (s_ ", ")
        (List.map kind_str knds)
    in

      fprintf fmt (f_ "### Plugin %s (%s)\n\n") nm all_kinds;
      begin
        match vo with 
          | Some ver -> 
              fprintf fmt (f_ "__Version__: %s<br/>\n\n") 
                (OASISVersion.string_of_version ver)
          | None -> 
              ()
      end;
      pp_help_replace ~loc:("plugin "^nm) extra_vars fmt hlp.OASISPlugin.help_template
  in

  let plugins = 
    let module MapGen =
      Map.Make
        (struct
           type t = string * OASISVersion.t option
           let compare (nm1, vo1) (nm2, vo2) =
             OASISPlugin.plugin_compare (`All, nm1, vo1) (`All, nm2, vo2)
         end)
    in
    let mp = 
      List.fold_left
        (fun mp (knd, nm, ver) ->
           let frmr =
             try 
               MapGen.find (nm, ver) mp
             with Not_found ->
               []
           in
             MapGen.add (nm, ver) (knd :: frmr) mp)
        MapGen.empty
        (OASISPlugin.all_plugins ())
    in
    let lst =
      MapGen.fold 
        (fun (nm, vo) knds acc ->
           (nm, knds, vo, OASISPlugin.help (`All, nm, vo)) :: acc)
        mp
        []
    in
      List.sort 
        (fun (nm1, _, vo1, {OASISPlugin.help_order = ord1}) 
               (nm2, _, vo2, {OASISPlugin.help_order = ord2}) ->
           match ord1 - ord2 with
             | 0 -> 
                 OASISPlugin.plugin_compare (`All, nm1, vo1) (`All, nm2, vo2)
             | n ->
                 n)
        lst
  in

    pp_help_replace 
      ([
        "ListShortLicenses",
        pp_short_licenses;

        "ListLicenseExceptions",
        pp_license_exceptions;

        "ListStandardVariables",
        (fun () ->
           pp_standard_variables env_display env_schm);

        (* TODO *)
        "ListFunctionVariables",
        (fun () -> 
           "TODO");

        (* TODO: vars provides this field ? *)
        "ListOASISBuildFields",
        (fun () -> 
           pp_section_fields 
             ~allowed_fields:build_section_fields 
             ?plugin 
             OASISLibrary.schema);

        "ListOASISLibraryFields",
        (fun () ->  
           pp_section_fields 
             ~allowed_fields:library_fields
             ?plugin 
             OASISLibrary.schema);

        "ListOASISExecutableFields",
        (fun () ->  
           pp_section_fields 
             ~allowed_fields:executable_fields
             ?plugin 
             OASISExecutable.schema);

        "OASISCommandLineHelp",
        (fun () -> 
           let fmt = 
             str_formatter 
           in
             pp_set_margin fmt 80;
             pp_print_cli_help fmt ();
             flush_str_formatter ());

        "ListOASISPlugins",
        (fun () ->
           let buff = 
             Buffer.create 13
           in
           let fmt = 
             formatter_of_buffer buff
           in
             pp_open_vbox fmt 0;
             pp_print_list
               (fun fmt e ->
                  pp_open_box fmt 0;
                  pp_plugin fmt e;
                  pp_close_box fmt ())
               "@,"
               fmt
               plugins;
             pp_close_box fmt ();
             Buffer.contents buff)
      ] @ (vars ()))
      fmt
      OASISData.readme_template_mkd
