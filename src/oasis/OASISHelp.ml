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


(* TODO: move this file to src/cli/Manual.ml *)


(** Display help for OASIS
    @author Sylvain Le Gall
*)


open OASISLicense
open OASISTypes
open OASISSchema_intern
open OASISGettext
open OASISUtils
open OASISFeatures
open Format
open FormatExt


let escape_markdown str =
  let buff = Buffer.create (String.length str) in
  String.iter
    (function
      | '*' -> Buffer.add_string buff "\\*"
      | '_' -> Buffer.add_string buff "\\_"
      | c -> Buffer.add_char buff c) str;
  Buffer.contents buff


let pp_print_escaped fmt str =
  pp_print_string_spaced fmt (escape_markdown str)


let fields_of_section ?plugin schm =
  List.rev
    (PropList.Schema.fold
       (fun acc key extra help ->
          match extra.kind, plugin with
            | StandardField, None
            | DefinePlugin _, None
            | DefinePlugins _, None ->
              (key, (help, extra)) :: acc
            | FieldFromPlugin plg, Some plg'  ->
              if OASISPlugin.plugin_compare plg' plg = 0 then
                (key, (help, extra)) :: acc
              else
                acc
            | FieldFromPlugin _, None
            | _, Some _ ->
              acc)
       []
       schm)


module Var =
struct
  type t =
    {
      values:  (unit -> string) MapString.t;
      used:    (string * bool ref) list;
      plugin:  ([`All | plugin_kind] plugin) option;
    }

  let create ?plugin () =
    {
      values = MapString.empty;
      used   = [];
      plugin = plugin;
    }

  let loc =
    function
      | Some plg ->
        Printf.sprintf " in plugin %s" (OASISPlugin.string_of_plugin plg)
      | None ->
        ""
  let add nm f t =
    let used =
      ref false
    in
    if MapString.mem nm t.values then
      failwithf
        (f_ "Help variable '%s' already defined%s")
        nm (loc t.plugin);
    {t with
       values = MapString.add
           nm
           (fun () -> used := true; f ())
           t.values;
       used   = (nm, used) :: t.used;
    }

  let find ~line nm t =
    try
      MapString.find nm t.values
    with Not_found ->
      failwithf
        (f_ "Unknown help variable %s in line '%s'%s")
        nm line (loc t.plugin)

  let check t =
    let lst =
      List.map fst
        (List.filter
           (fun (_, r) -> not !r)
           t.used)
    in
    match lst with
      | [] ->
        ()
      | lst ->
        failwithf
          (f_ "Unused help variable %s%s")
          (String.concat (s_ ", ") lst)
          (loc t.plugin)

  let of_list ?plugin lst =
    List.fold_left
      (fun t (nm, f) ->
         add nm f t)
      (create ?plugin ())
      lst
end


exception Empty of string


let pp_section_fields ?plugin ?allowed_fields schm nm =

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
  if fields = [] then
    raise (Empty nm);

  nm,
  fun () ->
    let fake_data =
      PropList.Data.create ()
    in

    let fmt =
      str_formatter
    in
    pp_set_margin fmt 80;
    pp_open_vbox fmt 0;
    pp_print_list
      (fun fmt (key, (help, extra)) ->
         let extra_info = [] in
         let extra_info =
           try
             let _s: string = PropList.Schema.get schm fake_data key in
             extra_info
           with
             | PropList.Not_set _ ->
               s_ "__mandatory__" :: extra_info
             | PropList.No_printer _ | OASISValues.Not_printable ->
               extra_info
         in
         let extra_info =
           match extra.feature with
             | Some feature ->
               let requirement =
                 match feature.publication with
                   | InDev stage ->
                     Printf.sprintf
                       (f_ "__require %s: %s__")
                       (field_of_stage stage)
                       (escape_markdown feature.name)
                   | SinceVersion ver ->
                     Printf.sprintf
                       (f_ "__since OASISFormat: %s__")
                       (OASISVersion.string_of_version ver)
               in
               requirement :: extra_info
             | None ->
               extra_info
         in

         match help, extra_info with
           | Some h, [] ->
             fprintf fmt (f_ " * @[`%s`: %a@]")
               key pp_print_escaped (h ())
           | Some h, lst ->
             fprintf fmt (f_ " * @[`%s`: %a (%s)@]")
               key pp_print_escaped (h ())
               (String.concat ", " extra_info)
           | None, [] ->
             fprintf fmt (f_ " * @[`%s`: <No help>@]") key
           | None, lst ->
             fprintf fmt (f_ " * @[`%s`: <No help> (%s)@]")
               key (String.concat ", " extra_info))
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
         pp_print_escaped fmt data.long_name
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
             pp_print_escaped txt
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
             pp_print_escaped txt
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
         pp_print_escaped fmt data.explanation
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
             pp_print_escaped descr)
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
let mk_std_vars ?plugin ?(filter=(fun _ -> true)) acc =
  let bn =
    match plugin with
      | None ->
        "OASIS"
      | Some (knd, nm, ver) ->
        (String.capitalize nm)^
          (String.capitalize (kind_str knd))
  in

  let add_if_valid schm vars (pre, suf) =
    let nm =
      pre^bn^suf
    in
    try
      let nm, f =
        pp_section_fields ?plugin schm nm
      in
      if filter (nm, f) then
        Var.add nm f vars
      else
        vars
    with Empty nm ->
      vars
  in

  List.fold_left
    (fun acc (pre, suf, add) ->
       add acc (pre, suf))
    acc
    [
      "List", "PackageFields",
      add_if_valid OASISPackage.schema;

      "List", "FlagFields",
      add_if_valid OASISFlag.schema;

      "List", "LibraryFields",
      add_if_valid OASISLibrary.schema;

      "List", "ObjectFields",
      add_if_valid OASISObject.schema;

      "List", "ExecutableFields",
      add_if_valid OASISExecutable.schema;

      "List", "DocumentFields",
      add_if_valid OASISDocument.schema;

      "List", "TestFields",
      add_if_valid OASISTest.schema;

      "List", "SourceRepositoryFields",
      add_if_valid OASISSourceRepository.schema;
    ]


let pp_list_all_features () =
  let fmt =
    str_formatter
  in
  let features =
    List.fold_left
      (fun acc feature ->
         match feature with
           | {publication = InDev stage}  ->
             (feature.name, stage, feature.description ()) :: acc
           | _ ->
             acc)
      [] (OASISFeatures.list ())
  in
  pp_set_margin fmt 80;
  pp_open_vbox fmt 0;
  pp_print_list
    (* TODO: add plugin after description. *)
    (fun fmt (name, stage, description) ->
       fprintf fmt (f_ " * @[`%s`: %a (%s)@]")
         name
         pp_print_escaped description
         (string_of_stage stage))
    "@,"
    fmt
    features;
  pp_close_box fmt ();
  flush_str_formatter ()


let pp_help_replace vars fmt str =
  let buff =
    Buffer.create 13
  in
  List.iter
    (fun str ->
       (* Replace variables *)
       Buffer.add_substitute
         buff
         (fun nm -> (Var.find ~line:str nm vars) ())
         str;
       pp_print_string fmt (Buffer.contents buff);
       pp_print_newline fmt ();
       Buffer.clear buff)
    str;
  Var.check vars


let pp_print_help ?plugin fmt pp_print_cli_help env_schm env_display =
  let build_section_fields, library_fields, object_fields, executable_fields =
    let set_fields_of_section schm =
      SetString.of_list
        (List.rev_map fst
           (fields_of_section ?plugin schm.schm))
    in
    let lib_flds = set_fields_of_section OASISLibrary.schema in
    let obj_flds = set_fields_of_section OASISObject.schema in
    let exec_flds = set_fields_of_section OASISExecutable.schema in
    let common_flds =
      SetString.inter (SetString.inter lib_flds obj_flds) exec_flds
    in
    common_flds,
    SetString.diff lib_flds common_flds,
    SetString.diff obj_flds common_flds,
    SetString.diff exec_flds common_flds
  in

  let pp_plugin fmt (nm, knds, vo, hlp) =

    (* Create additional variables that match the different
     * plugin kind.
    *)
    let mk_derived_vars acc knd =
      let plugin =
        knd, nm, vo
      in
      mk_std_vars ~plugin acc
    in
    let plugin =
      `All, nm, vo
    in
    let vars =
      List.fold_left
        mk_derived_vars
        (Var.create ~plugin ())
        knds
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
    pp_help_replace vars fmt hlp.OASISPlugin.help_template
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

  let vars =
    Var.of_list
      [

        "ListAllFeatures",
        pp_list_all_features;

        "ListShortLicenses",
        pp_short_licenses;

        "ListLicenseExceptions",
        pp_license_exceptions;

        "ListStandardVariables",
        (fun () ->
           pp_standard_variables env_display env_schm);

        (* TODO
           "ListFunctionVariables",
           (fun () ->
           "TODO");
        *)

        pp_section_fields
          ~allowed_fields:build_section_fields
          ?plugin
          OASISLibrary.schema
          "ListOASISBuildFields";

        pp_section_fields
          ~allowed_fields:library_fields
          ?plugin
          OASISLibrary.schema
          "ListOASISLibraryFields";

        pp_section_fields
          ~allowed_fields:object_fields
          ?plugin
          OASISObject.schema
          "ListOASISObjectFields";

        pp_section_fields
          ~allowed_fields:executable_fields
          ?plugin
          OASISExecutable.schema
          "ListOASISExecutableFields";

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
           Buffer.contents buff);

        "ListOASISTests",
        (fun () ->
           String.concat
             "\n"
             (List.map
                (fun et ->
                   Printf.sprintf "* `%s(X)`"
                     (OASISExpr.string_of_test et))
                OASISExpr.tests));
      ]
  in
  let vars =
    mk_std_vars
      ~filter:(fun (nm, _) ->
        (* These three variables are handled directly, to make
         * a difference with common build fields.
        *)
        nm <> "ListOASISLibraryFields" &&
        nm <> "ListOASISObjectFields" &&
        nm <> "ListOASISExecutableFields")
      vars
  in

  pp_help_replace vars fmt OASISData.readme_template_mkd
