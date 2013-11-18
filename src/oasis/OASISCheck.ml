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


(** Check an OASIS package
  *)


open OASISGettext
open OASISSchema_intern
open OASISUtils
open OASISPlugin
open OASISTypes
open PropList


let check_schema ~ctxt where schm oasis_version data =

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

  let check_get schm data fld msgfld =
    try
      let _ =
        Schema.get schm data fld
      in
        msgfld
    with
      | Not_set _ ->
          fld :: msgfld
      | No_printer _ ->
          msgfld
      | OASISValues.Not_printable ->
          msgfld
  in

  (** Check all mandatory fields are set. *)

  let plugins, msgfld =
    Schema.fold
      (fun ((plugins, msgfld) as acc) fld extra hlp ->
         match extra.kind with
           | DefinePlugin knd ->
               begin
                 try
                   let id =
                     plugin_of_string knd (Schema.get schm data fld)
                   in
                     SetPlugin.add id plugins,
                     msgfld
                 with _ ->
                   plugins,
                   check_get schm data fld msgfld
               end


           | DefinePlugins knd ->
               begin
                 try
                   let lst =
                     plugins_of_string knd (Schema.get schm data fld)
                   in
                     List.fold_left
                       (fun acc id -> SetPlugin.add id acc)
                       plugins lst,
                     msgfld

                 with _ ->
                   plugins,
                   check_get schm data fld msgfld
               end

           | StandardField
           | FieldFromPlugin _ ->
               acc)
      (SetPlugin.empty, [])
      schm
  in

  let msgfld =
    Schema.fold
      (fun acc fld extra hlp ->
         match extra.kind with
           | DefinePlugin _ | DefinePlugins _ ->
               begin
                 (* Already checked before *)
                 acc
               end

           | StandardField ->
               begin
                 check_get schm data fld acc
               end

           | FieldFromPlugin plg_id ->
               begin
                 if SetPlugin.mem plg_id plugins then
                   begin
                     check_get schm data fld acc
                   end

                 else if check_is_set schm data fld &&
                         not (check_is_default schm data fld) then
                   begin
                     OASISMessage.warning ~ctxt
                       (f_ "Field %s is set but matching plugin is not \
                            enabled.")
                       fld;
                     acc
                   end

                 else
                   begin
                     acc
                   end

               end)
      msgfld
      schm
  in

  let () =
    if msgfld <> [] then
      failwithf (f_ "Missing field in %s: %s")
        where
        (String.concat (s_ ", ") msgfld)

  in

  (** Check that all fields set are ok with OASISFormat. *)

  let () =
    let sov = OASISVersion.string_of_version in
    Schema.fold
      (fun () fld extra _ ->
         if check_is_set schm data fld then
           match extra.since_version with
             | Some ver ->
                 begin
                   match extra.kind with
                     | DefinePlugin _ | DefinePlugins _
                     | StandardField ->
                         if not (OASISVersion.comparator_apply
                                   oasis_version
                                   (OASISVersion.VGreaterEqual ver)) then
                           failwithf
                             (f_ "Field '%s' in %s is only valid since \
                                OASIS v%s, update OASISFormat field from '%s' \
                                to '%s' after checking OASIS changelog.")
                             fld where
                             (sov ver)
                             (sov oasis_version)
                             (sov ver)

                     | FieldFromPlugin plg_id ->
                         let plugin_name, plugin_version =
                           match plg_id with
                             | _, nm, Some ver ->
                                 nm, ver
                             | _, plugin_name, None ->
                                 failwithf
                                   (f_ "Field '%s' in %s is only valid for
                                      the OASIS plugin %s since v%s, \
                                      but no plugin version is defined in \
                                      the _oasis file, change '%s' to \
                                      '%s (%s)' in your _oasis file.")
                                   fld where plugin_name (sov ver)
                                   plugin_name
                                   plugin_name (sov ver)
                         in
                           if not (OASISVersion.comparator_apply
                                     plugin_version
                                     (OASISVersion.VGreaterEqual ver)) then
                             failwithf
                               (f_ "Field '%s' in %s is only valid for \
                                  the OASIS plugin %s since v%s, \
                                  update your plugin from \
                                  '%s (%s)' to '%s (%s)' after \
                                  checking the plugin's changelog.")
                               fld where plugin_name (sov ver)
                               plugin_name (sov plugin_version)
                               plugin_name (sov ver)
                 end

             | None ->
                 ())
      ()
      schm
  in
    ()


let check_package ~ctxt pkg =

  let standard_vars =
    if OASISVersion.version_0_3_or_after pkg.oasis_version then
      set_string_of_list
        ["tests";
         "docs"]
    else
      SetString.empty
  in

  (** Check that there is no overlap in variable name. *)
  let _mp : string MapString.t =
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



