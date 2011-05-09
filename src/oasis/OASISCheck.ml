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

(** Check an OASIS package
  *)

open OASISGettext
open OASISSchema_intern
open OASISUtils
open OASISPlugin
open OASISTypes
open PropList

let check_schema ~ctxt where schm data =

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
    try
      let _ = 
        Schema.get schm data fld
      in 
        true 
    with 
      | Not_set _ ->
          false
      | No_printer _
      | OASISValues.Not_printable ->
          true
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
                       (f_ "Field %s is set but matching plugin is not enabled.")
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
    if msgfld <> [] then
      failwithf (f_ "Missing field in %s: %s")
        where
        (String.concat (s_ ", ") msgfld)
