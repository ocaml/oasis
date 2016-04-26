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


open BaseEnv
open OASISUtils
open OASISGettext


module SMap = Map.Make(String)


let ocamlc =
  BaseCheck.prog_opt "ocamlc"


let ocamlc_config_map =
  (* Map name to value for ocamlc -config output
     (name ^": "^value)
  *)
  let rec split_field mp lst =
    match lst with
      | line :: tl ->
        let mp =
          try
            let pos_semicolon =
              String.index line ':'
            in
            if pos_semicolon > 1 then
              (
                let name =
                  String.sub line 0 pos_semicolon
                in
                let linelen =
                  String.length line
                in
                let value =
                  if linelen > pos_semicolon + 2 then
                    String.sub
                      line
                      (pos_semicolon + 2)
                      (linelen - pos_semicolon - 2)
                  else
                    ""
                in
                SMap.add name value mp
              )
            else
              (
                mp
              )
          with Not_found ->
            (
              mp
            )
        in
        split_field mp tl
      | [] ->
        mp
  in

  let cache =
    lazy
      (var_protect
         (Marshal.to_string
            (split_field
               SMap.empty
               (OASISExec.run_read_output
                  ~ctxt:!BaseContext.default
                  (ocamlc ()) ["-config"]))
            []))
  in
  var_redefine
    "ocamlc_config_map"
    ~hide:true
    ~dump:false
    (fun () ->
       (* TODO: update if ocamlc change !!! *)
       Lazy.force cache)


let var_define nm =
  (* Extract data from ocamlc -config *)
  let avlbl_config_get () =
    Marshal.from_string
      (ocamlc_config_map ())
      0
  in
  let chop_version_suffix s =
    try
      String.sub s 0 (String.index s '+')
    with _ ->
      s
  in

  let nm_config, value_config =
    match nm with
      | "ocaml_version" ->
        "version", chop_version_suffix
      | _ -> nm, (fun x -> x)
  in
  var_redefine
    nm
    (fun () ->
       try
         let map =
           avlbl_config_get ()
         in
         let value =
           SMap.find nm_config map
         in
         value_config value
       with Not_found ->
         failwithf
           (f_ "Cannot find field '%s' in '%s -config' output")
           nm
           (ocamlc ()))

