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
open OASISSchema
open OASISGettext
open OASISUtils
open Format
open FormatExt

let fields_of_section ?plugin schm =
  List.rev
    (PropList.Schema.fold
       (fun acc key extra help ->
          match extra.kind with
            | StandardField | DefinePlugin _ | DefinePlugins _ ->
                if plugin = None then
                  (key, help) :: acc
                else
                  acc
            | FieldFromPlugin plg ->
                if plugin = Some plg then
                  (key, help) :: acc
                else
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

(** Standard variables to replace in help files *)
let vars ?plugin () =
  [
    "ListOASISTests",
    (fun () ->
       String.concat 
         "\n"
         (List.map
            (fun et ->
               Printf.sprintf "* `%s(X)`" 
                 (OASISExpr.string_of_test et))
            OASISExpr.tests));

    "ListOASISPackageFields",
    (fun () -> pp_section_fields ?plugin OASISPackage.schema);

    "ListOASISFlagFields",
    (fun () -> pp_section_fields ?plugin OASISFlag.schema);

    "ListOASISBuildFields",
    (fun () -> pp_section_fields ?plugin OASISLibrary.schema);

    "ListOASISLibraryFields",
    (fun () -> pp_section_fields ?plugin OASISLibrary.schema);
    
    "ListOASISExecutableFields",
    (fun () -> pp_section_fields ?plugin OASISExecutable.schema);

    "ListOASISDocumentFields",
    (fun () -> pp_section_fields ?plugin OASISDocument.schema);

    "ListOASISTestFields",
    (fun () -> pp_section_fields ?plugin OASISTest.schema);

    "ListOASISSourceRepositoryFields",
    (fun () -> pp_section_fields ?plugin OASISSourceRepository.schema);
  ]

let pp_help_replace vars fmt str = 
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
                failwithf2
                  (f_ "Unknown variable %s in documentation (line is '%s')")
                  nm
                  str)
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

  let pp_plugin fmt (plg, ver, hlp, hlp_xtr_vr) = 
    let kinds = 
      let check_add (fnd, nm) acc = 
       try
         let _ = 
           fnd (plg, Some ver)
         in
           nm :: acc
       with Failure _ ->
         acc
      in
        List.fold_right
          (fun f acc -> f acc)
          [
            check_add (OASISPlugin.Configure.find, "conf");
            check_add (OASISPlugin.Build.find,     "build");
            check_add (OASISPlugin.Doc.find,       "doc");
            check_add (OASISPlugin.Test.find,      "test");
            check_add (OASISPlugin.Install.find,   "install");
            check_add (OASISPlugin.Extra.find,     "extra");
          ]
          []
    in
      fprintf fmt (f_ "### Plugin %s\n\n") plg;
      fprintf fmt (f_ "__Version__: %s<br/>\n") ver;
      fprintf fmt (f_ "__Types__: %s\n\n") (String.concat (s_ ", ") kinds);
      pp_help_replace (hlp_xtr_vr @ (vars ~plugin:plg ())) fmt hlp;
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
           let plgs =
             let all = 
               OASISPlugin.MapPlugin.fold
                 (fun k (v, o, a, b) acc ->
                    (o, (k, v, a, b)) :: acc)
                 !OASISPlugin.help_all
                 []
             in
               List.map snd (List.sort compare all)
           in
             pp_open_vbox fmt 0;
             pp_print_list
               (fun fmt e ->
                  pp_open_box fmt 0;
                  pp_plugin fmt e;
                  pp_close_box fmt ())
               "@,"
               fmt
               plgs;
             pp_close_box fmt ();
             Buffer.contents buff)
      ] @ (vars ()))
      fmt
      OASISData.readme_template_mkd
