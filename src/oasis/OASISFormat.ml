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


open OASISSchema_intern
open OASISTypes
open Format
open FormatExt


(** Pretty printing of OASIS files
*)


let pp_print_fields fmt (schm, _, data) =
  let fake_data =
    PropList.Data.create ()
  in
  let key_value =
    List.rev
      (PropList.Schema.fold
         (fun acc key extra _ ->
            try
              let str =
                PropList.Schema.get
                  schm
                  data
                  key
              in
              let is_default =
                try
                  let default =
                    PropList.Schema.get
                      schm
                      fake_data
                      key
                  in
                  str = default
                with
                  | OASISValues.Not_printable
                  | PropList.Not_set _ ->
                    (* Unable to compare so this is not default *)
                    false
              in
              if not is_default then
                (key, str) :: acc
              else
                acc
            with
              | OASISValues.Not_printable ->
                acc
              | PropList.Not_set _ ->
                (* TODO: is it really necessary *)
                (* when extra. <> None ->*)
                acc)
         []
         schm)
  in

  let max_key_length =
    (* ":" *)
    1
    +
      (* Maximum length of a key *)
      (List.fold_left
         max
         0

         (* Only consider length of key *)
         (List.rev_map
            fst

            (* Remove key/value that exceed line length *)
            (List.filter
               (fun (k, v) -> k + v < pp_get_margin fmt ())

               (* Consider only length of key/value *)
               (List.rev_map
                  (fun (k, v) -> String.length k, String.length v)
                  key_value))))
  in

  pp_open_vbox fmt 0;
  List.iter
    (fun (k, v) ->
       pp_open_box fmt 2;
       pp_print_string fmt k;
       pp_print_string fmt ":";
       pp_print_break fmt (max 0 (max_key_length - String.length k)) 0;
       pp_print_string_spaced fmt v;
       pp_close_box fmt ();
       pp_print_cut fmt ())
    key_value;
  pp_close_box fmt ()


let pp_print_section plugins fmt sct =
  let pp_print_section' schm t =
    let (schm, _, _) as sct_data =
      OASISSchema_intern.to_proplist schm plugins t
    in

    let {cs_name = nm; cs_data = data} =
      OASISSection.section_common sct
    in

    let pp_id_or_string fmt str =
      (* A string is an id if varname_of_string doesn't change it *)
      if OASISUtils.is_varname str then
        fprintf fmt "%s" str
      else
        fprintf fmt "%S" str
    in
    fprintf fmt "@[<v 2>%s %a@,%a@]@,"
      (PropList.Schema.name schm)
      pp_id_or_string nm
      pp_print_fields sct_data
  in

  match sct with
    | Library (cs, bs, lib) ->
      pp_print_section' OASISLibrary.schema (cs, bs, lib)
    | Object (cs, bs, obj) ->
      pp_print_section' OASISObject.schema (cs, bs, obj)
    | Executable (cs, bs, exec) ->
      pp_print_section' OASISExecutable.schema (cs, bs, exec)
    | SrcRepo (cs, src_repo) ->
      pp_print_section' OASISSourceRepository.schema (cs, src_repo)
    | Test (cs, test) ->
      pp_print_section' OASISTest.schema (cs, test)
    | Flag (cs, flag) ->
      pp_print_section' OASISFlag.schema (cs, flag)
    | Doc (cs, doc) ->
      pp_print_section' OASISDocument.schema (cs, doc)


let pp_print_package fmt pkg =

  let (_, plugins, _) as pkg_data =
    OASISSchema_intern.to_proplist OASISPackage.schema [] pkg
  in

  pp_open_vbox fmt 0;

  pp_print_fields fmt pkg_data;
  pp_print_cut fmt ();

  List.iter (pp_print_section plugins fmt) pkg.sections;

  pp_close_box fmt ()

