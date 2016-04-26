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


(** Check an OASIS package
*)


open OASISGettext
open OASISSchema_intern
open OASISUtils
open OASISPlugin
open OASISTypes
open PropList


let check_schema ~ctxt where schm plugins features_data data =

  let check_is_default schm data fld =
    let fake_data =
      Data.create ()
    in
    try
      (Schema.get schm data fld) = (Schema.get schm fake_data fld)
    with
      | Not_set _
      | No_printer _
      | OASISValues.Not_printable ->
        (* TODO: Don't know what to answer *)
        true
  in

  let check_is_set schm data fld =
    let field_set = Data.elements data in
    List.mem fld field_set
  in

  (* Collect plugins and their version. *)
  let plugins =
    Schema.fold
      (fun plugins fld extra hlp ->
         match extra.kind with
           | DefinePlugin knd ->
             begin
               try
                 let id =
                   plugin_of_string knd (Schema.get schm data fld)
                 in
                 SetPlugin.add id plugins
               with _ ->
                 plugins
             end


           | DefinePlugins knd ->
             begin
               try
                 let lst =
                   plugins_of_string knd (Schema.get schm data fld)
                 in
                 List.fold_left
                   (fun acc id -> SetPlugin.add id acc)
                   plugins lst
               with _ ->
                 plugins
             end

           | StandardField | FieldFromPlugin _ ->
             plugins)
      plugins schm
  in

  (* Inject plugin data in features data. *)
  let features_data =
    SetPlugin.fold
      (fun plg_id features_data ->
         OASISFeatures.Data.add_plugin plg_id features_data)
      plugins features_data
  in

  (* Check all mandatory fields are set. *)
  let msgfld =
    let check_get schm data fld msgfld =
      try
        let _ = Schema.get schm data fld in msgfld
      with
        | Not_set _ -> fld :: msgfld
        | No_printer _ -> msgfld
        | OASISValues.Not_printable -> msgfld
    in

    Schema.fold
      (fun acc fld extra hlp ->
         match extra.kind with
           | DefinePlugin _ | DefinePlugins _ | StandardField ->
             check_get schm data fld acc

           | FieldFromPlugin ((_, nm, ver) as plg_id) ->
             if mem_no_version plg_id plugins then begin
               check_get schm data fld acc
             end else if check_is_set schm data fld &&
                         not (check_is_default schm data fld) then begin
               OASISMessage.warning ~ctxt
                 (f_ "Field %s is set but matching plugin %s is not \
                      enabled.")
                 fld nm;
               acc
             end else begin
               acc
             end)
      [] schm
  in

  let () =
    if msgfld <> [] then
      failwithf (f_ "Missing field in %s: %s")
        where
        (String.concat (s_ ", ") msgfld)
  in

  (** Check that all fields set are ok with OASISFormat. *)

  let () =
    Schema.fold
      (fun () fld extra _ ->
         if check_is_set schm data fld then
           match extra.feature with
             | Some feature ->
               OASISFeatures.data_assert feature features_data
                 (OASISFeatures.Field (fld, where))
             | None ->
               ())
      ()
      schm
  in

  plugins


let check_package ~ctxt pkg =

  let standard_vars =
    SetString.of_list
      (List.flatten
         [
           if OASISFeatures.package_test OASISFeatures.flag_docs pkg then
             ["tests"]
           else
             [];

           if OASISFeatures.package_test OASISFeatures.flag_tests pkg then
             ["docs"]
           else
             [];
         ])
  in

  (** Check that there is no overlap in variable name. *)
  let _mp: string MapString.t =
    List.fold_left
      (fun mp ->
         function
           | Flag (cs, _)
           | Executable (cs, _, _) as sct ->
             let sct_str = OASISSection.string_of_section sct in
             let varname = cs.cs_name in
             if SetString.mem varname standard_vars then
               OASISMessage.warning ~ctxt
                 (f_ "%s define variable '%s' which is also a standard \
                      variable, possible conflict.")
                 sct_str varname;
             if MapString.mem varname mp then
               OASISMessage.warning ~ctxt
                 (f_ "%s define variable '%s' which is also defined by \
                      %s, possible conflict.")
                 sct_str varname (MapString.find varname mp);
             MapString.add varname sct_str mp
           | Library _ | Object _ | Doc _ | SrcRepo _ | Test _ ->
             mp)
      MapString.empty
      pkg.sections
  in

  ()
