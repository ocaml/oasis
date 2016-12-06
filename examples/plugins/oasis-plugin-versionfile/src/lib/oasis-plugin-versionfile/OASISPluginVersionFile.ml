(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2016, Sylvain Le Gall                                   *)
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

open OASISPlugin
open OASISPluginVersionFileGettext
open OASISValues
open OASISSchema
open OASISTypes

type t =
  {
    filename: string;
  }

let version_short = OASISVersion.chop OASISPluginVersionFileConf.version
let plugin = `Extra, "VersionFile", Some version_short
let self_id, all_id = Extra.create plugin

let feature_name =
  OASISFeatures.create "filename" ~plugin
    (OASISFeatures.since_version "0.1")
    (fun () ->
       s_ "Allow to choose which file to generate.")

let pivot_data = data_new_property plugin

let generator =
  let new_field nm =
    new_field OASISPackage.schema all_id nm
  in
  let filename =
    new_field
      "Filename"
      ~default:"version.ml"
      ~feature:feature_name
      string
      (fun () ->
         s_ "File to generate where the version will be stored")
      pivot_data (fun _ t -> t.filename)
  in

  fun data ->
    {
      filename = filename data;
    }

let main ctxt pkg =
  let t = generator pkg.schema_data in
  let content =
    Printf.sprintf
      "let version = %S"
      (OASISVersion.string_of_version pkg.version)
  in
  let open OASISFileTemplate in
  OASISPlugin.add_file
    (template_of_mlfile
       t.filename
       [] (* header *)
       [content] (* body *)
       [] (* footer *))
    ctxt

let init () =
  Extra.register_act self_id main;
  register_help
    plugin
    {(help_default OASISPluginVersionFileData.readme_template_mkd) with
       help_order = 40};
  register_generator_package all_id pivot_data generator

let () = init ()
