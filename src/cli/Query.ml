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


(** Load and query _oasis file
    @author Sylvain Le Gall
*)


open CLISubCommand
open BaseMessage
open Genlex
open OASISGettext
open OASISTypes
open OASISSection
open OASISUtils


let lexer =
  make_lexer
    ["ListSections"; "ListFields"; "("; ")"; "."]


let query pkg separator str =
  let proplist_schema schm =
    (* TODO: oops access to unpublished module _intern *)
    schm.OASISSchema_intern.schm
  in

  let assoc_sections =
    [
      "Library", (`Library, proplist_schema OASISLibrary.schema);

      "Executable", (`Executable, proplist_schema OASISExecutable.schema);

      "Flag", (`Flag, proplist_schema OASISFlag.schema);

      "SrcRepo", (`SrcRepo, proplist_schema OASISSourceRepository.schema);

      "Test", (`Test, proplist_schema OASISTest.schema);

      "Doc", (`Doc, proplist_schema OASISDocument.schema);
    ]
  in

  let mk_section sct =
    let knd, nm =
      OASISSection.section_id sct
    in
    let start, (_, schm) =
      List.find
        (fun (str, (knd', _)) -> knd = knd')
        assoc_sections
    in
    let fmt =
      if OASISUtils.is_varname nm then
        Printf.sprintf "%s(%s)"
      else
        Printf.sprintf "%s(%S)"
    in
    fmt start nm, schm
  in

  let parse_id_or_string =
    parser
  | [< 'Ident str >] ->
    str
  | [< 'String str >] ->
    str
  in

  let parse_fld_or_section start_nm =
    parser
  | [< 'Kwd "("; nm = parse_id_or_string; 'Kwd ")";
       'Kwd ".";
       fld = parse_id_or_string >] ->
    begin
      let kind, schm =
        try
          List.assoc
            (String.lowercase start_nm)
            (List.map
               (fun (nm, e) -> String.lowercase nm, e)
               assoc_sections)
        with Not_found ->
          failwithf
            (f_ "Don't know section kind '%s' in query '%s'")
            start_nm str
      in
      let sct  =
        OASISSection.section_find
          (kind, nm)
          pkg.sections
      in
      let data =
        (OASISSection.section_common sct).cs_data
      in
      schm, data, fld
    end

  | [< >] ->
    begin
      (* We have a single field *)
      (proplist_schema OASISPackage.schema),
      pkg.schema_data,
      start_nm
    end
  in

  let parse =
    parser
  | [< start_nm = parse_id_or_string;
       (schm, data, fld) = parse_fld_or_section start_nm >] ->
    begin
      PropList.Schema.get schm data fld
    end

  | [< 'Kwd "ListSections" >] ->
    begin
      String.concat
        separator
        (List.map
           (fun sct -> fst (mk_section sct))
           pkg.sections)
    end

  | [< 'Kwd "ListFields" >] ->
    begin
      let fold_schm prefix schm data acc =
        PropList.Schema.fold
          (fun acc nm extra _ ->
             try
               let _v: string =
                 PropList.Schema.get schm data nm
               in
               match extra.OASISSchema_intern.feature with
                 | Some ftr ->
                   if OASISFeatures.package_test ftr pkg then
                     (prefix^nm) :: acc
                   else
                     acc
                 | None -> (prefix^nm) :: acc
             with e ->
               acc)
          acc
          schm
      in
      let lst =
        List.fold_left
          (fun acc sct ->
             let prefix, schm = mk_section sct in
             let data = (section_common sct).cs_data in
             fold_schm (prefix^".") schm data acc)

          (* Start with the package fields *)
          (fold_schm "" (proplist_schema OASISPackage.schema)
             pkg.schema_data [])

          (* Continue with section fields *)
          pkg.sections
      in

      String.concat separator (List.rev lst)
    end

  in
  parse (lexer (Stream.of_string str))


let main ~ctxt (queries, separator) _ pkg =
  let answers =
    List.rev_map (query pkg separator) queries
  in
  print_endline (String.concat separator answers)


let () =
  CLISubCommand.register "query"
    ~usage:(ns_ "[options*] query*")
    (ns_ "Query an _oasis file")
    CLIData.query_mkd
    (CLICommon.parse_oasis_fn
       (CLISubCommand.make_run
          (fun () ->
             let separator = ref "\n" in
             let queries = ref [] in
             (["-separator",
               Arg.Set_string separator,
               s_ "str String to add between answers."],
              (fun e -> queries := e :: !queries)),
             (fun () -> !queries, !separator))
          main))

