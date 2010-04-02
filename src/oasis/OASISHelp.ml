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

(** Display help for OASIS
    @author Sylvain Le Gall
  *)

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
          if plugin = extra.plugin then
            (key, help) :: acc
          else
            acc)
       []
       schm)

let pp_section_fields ?plugin ?allowed_fields schm = 

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

let pp_cmd_usage args msg = 
  let fmt =
    str_formatter 
  in
  let pp_cmd_arg fmt (cli, t, hlp) = 
    let arg, hlp =
      match split ' ' hlp with
        | hd :: tl ->
            hd, (String.concat " " tl)
        | [] ->
            "", ""
    in
    let arg =
      match t with 
        | Arg.Symbol (lst, _) ->
            "{"^(String.concat "|" lst)^"}"
        | _ ->
            arg
    in
      if arg <> "" then
        fprintf fmt (f_ "@[`%s %s`: %a@]")
          cli
          arg
          pp_print_string_spaced hlp
      else
        fprintf fmt (f_ "@[`%s`: %a@]")
          cli
          pp_print_string_spaced hlp
  in
    pp_set_margin fmt 80;
    pp_open_vbox fmt 0;
    pp_open_box fmt 0;
    pp_print_string_spaced fmt msg;
    pp_close_box fmt ();
    pp_print_cut fmt ();
    pp_print_cut fmt ();
    pp_print_list pp_cmd_arg "@,@," fmt args;
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
                 (OASISExpr.string_of_expr_test et))
            OASISExpr.expr_tests));

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

    "ListOASISDocumentationFields",
    (fun () -> pp_section_fields ?plugin OASISDocumentation.schema);

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
                  (f_ "Unknown variable %s in documentationi (line is '%s')")
                  nm
                  str)
           str;
         pp_print_string fmt (Buffer.contents buff);
         pp_print_newline fmt ();
         Buffer.clear buff)
      str

let pp_help ?plugin fmt args msg =
  let build_section_fields, library_fields, executable_fields =
    let set_fields_of_section schm = 
      set_string_of_list (List.rev_map fst (fields_of_section ?plugin schm))
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
        (fun () -> pp_cmd_usage args msg);

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
