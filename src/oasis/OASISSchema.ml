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


open OASISTypes


type 'a t = 'a OASISSchema_intern.t


(** Create field name derived from a plugin
*)
let make_field_name (_, nm', _) nm =
  "X"^nm'^nm


(** Synchronize between plugin datastructure and PropList.Data.t
*)
let sync_proxy schema t nm (_, get) sync ds =
  try
    let t' =
      (* Extract plugin data from datastructure *)
      get (ref (schema.OASISSchema_intern.plugin ds))
    in
    sync ds t'
  with e ->
    Printf.eprintf
      "Field %S in schema %S: %s\n"
      (make_field_name t nm)
      (PropList.Schema.name schema.OASISSchema_intern.schm)
      (Printexc.to_string e);
    raise OASISValues.Not_printable


let new_field schema t nm ?default ?feature parse hlp pivot_data sync =
  let plugin =
    OASISPlugin.to_plugin t
  in
  OASISSchema_intern.new_field
    schema
    (make_field_name plugin nm)
    ?default
    ?feature
    ~plugin
    parse
    hlp
    (sync_proxy schema plugin nm pivot_data sync)


let new_field_conditional
    schema
    t
    nm
    ?default_cond
    ?default
    ?feature
    parse
    hlp
    pivot_data
    sync =
  let plugin =
    OASISPlugin.to_plugin t
  in
  OASISSchema_intern.new_field_conditional
    schema
    (make_field_name plugin nm)
    ?default_cond
    ?default
    ?feature
    ~plugin
    parse
    hlp
    (sync_proxy schema plugin nm pivot_data sync)

