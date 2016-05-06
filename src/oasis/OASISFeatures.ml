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

open OASISUtils
open OASISGettext
open OASISVersion

module MapPlugin =
  Map.Make
    (struct
      type t = OASISTypes.plugin_kind * OASISTypes.name
      let compare = Pervasives.compare
    end)

module Data =
struct
  type t =
    {
      oasis_version: OASISVersion.t;
      plugin_versions: OASISVersion.t option MapPlugin.t;
      alpha_features: string list;
      beta_features: string list;
    }

  let create oasis_version alpha_features beta_features =
    {
      oasis_version = oasis_version;
      plugin_versions = MapPlugin.empty;
      alpha_features = alpha_features;
      beta_features = beta_features
    }

  let of_package pkg =
    create
      pkg.OASISTypes.oasis_version
      pkg.OASISTypes.alpha_features
      pkg.OASISTypes.beta_features

  let add_plugin (plugin_kind, plugin_name, plugin_version) t =
    {t with
       plugin_versions = MapPlugin.add
           (plugin_kind, plugin_name)
           plugin_version
           t.plugin_versions}

  let plugin_version plugin_kind plugin_name t =
    MapPlugin.find (plugin_kind, plugin_name) t.plugin_versions

  let to_string t =
    Printf.sprintf
      "oasis_version: %s; alpha_features: %s; beta_features: %s; \
       plugins_version: %s"
      (OASISVersion.string_of_version t.oasis_version)
      (String.concat ", " t.alpha_features)
      (String.concat ", " t.beta_features)
      (String.concat ", "
         (MapPlugin.fold
            (fun (_, plg) ver_opt acc ->
               (plg^
                  (match ver_opt with
                    | Some v ->
                      " "^(OASISVersion.string_of_version v)
                    | None -> ""))
               :: acc)
            t.plugin_versions []))
end

type origin =
  | Field of string * string
  | Section of string
  | NoOrigin

type stage = Alpha | Beta


let string_of_stage =
  function
    | Alpha -> "alpha"
    | Beta -> "beta"


let field_of_stage =
  function
    | Alpha -> "AlphaFeatures"
    | Beta -> "BetaFeatures"

type publication = InDev of stage | SinceVersion of OASISVersion.t

type t =
  {
    name: string;
    plugin: OASISTypes.all_plugin option;
    publication: publication;
    description: unit -> string;
  }

(* TODO: mutex protect this. *)
let all_features = Hashtbl.create 13


let since_version ver_str = SinceVersion (version_of_string ver_str)
let alpha = InDev Alpha
let beta = InDev Beta


let to_string t =
  Printf.sprintf
    "feature: %s; plugin: %s; publication: %s"
    t.name
    (match t.plugin with
      | None -> "<none>"
      | Some (_, nm, _) -> nm)
    (match t.publication with
      | InDev stage -> string_of_stage stage
      | SinceVersion ver -> ">= "^(OASISVersion.string_of_version ver))

let data_check t data origin =
  let no_message = "no message" in

  let check_feature features stage =
    let has_feature = List.mem t.name features in
    if not has_feature then
      match origin with
        | Field (fld, where) ->
          Some
            (Printf.sprintf
               (f_ "Field %s in %s is only available when feature %s \
                    is in field %s.")
               fld where t.name (field_of_stage stage))
        | Section sct ->
          Some
            (Printf.sprintf
               (f_ "Section %s is only available when features %s \
                    is in field %s.")
               sct t.name (field_of_stage stage))
        | NoOrigin ->
          Some no_message
    else
      None
  in

  let version_is_good ~min_version version fmt =
    let version_is_good =
      OASISVersion.comparator_apply
        version (OASISVersion.VGreaterEqual min_version)
    in
    Printf.ksprintf
      (fun str ->
         if version_is_good then
           None
         else
           Some str)
      fmt
  in

  match origin, t.plugin, t.publication with
    | _, _, InDev Alpha -> check_feature data.Data.alpha_features Alpha
    | _, _, InDev Beta -> check_feature data.Data.beta_features Beta
    | Field(fld, where), None, SinceVersion min_version ->
      version_is_good ~min_version data.Data.oasis_version
        (f_ "Field %s in %s is only valid since OASIS v%s, update \
             OASISFormat field from '%s' to '%s' after checking \
             OASIS changelog.")
        fld where (string_of_version min_version)
        (string_of_version data.Data.oasis_version)
        (string_of_version min_version)

    | Field(fld, where), Some(plugin_knd, plugin_name, _),
      SinceVersion min_version ->
      begin
        try
          let plugin_version_current =
            try
              match Data.plugin_version plugin_knd plugin_name data with
                | Some ver -> ver
                | None ->
                  failwithf
                    (f_ "Field %s in %s is only valid for the OASIS \
                         plugin %s since v%s, but no plugin version is \
                         defined in the _oasis file, change '%s' to \
                         '%s (%s)' in your _oasis file.")
                    fld where plugin_name (string_of_version min_version)
                    plugin_name
                    plugin_name (string_of_version min_version)
            with Not_found ->
              failwithf
                (f_ "Field %s in %s is only valid when the OASIS plugin %s \
                     is defined.")
                fld where plugin_name
          in
          version_is_good ~min_version plugin_version_current
            (f_ "Field %s in %s is only valid for the OASIS plugin %s \
                 since v%s, update your plugin from '%s (%s)' to \
                 '%s (%s)' after checking the plugin's changelog.")
            fld where plugin_name (string_of_version min_version)
            plugin_name (string_of_version plugin_version_current)
            plugin_name (string_of_version min_version)
        with Failure msg ->
          Some msg
      end

    | Section sct, None, SinceVersion min_version ->
      version_is_good ~min_version data.Data.oasis_version
        (f_ "Section %s is only valid for since OASIS v%s, update \
             OASISFormat field from '%s' to '%s' after checking OASIS \
             changelog.")
        sct (string_of_version min_version)
        (string_of_version data.Data.oasis_version)
        (string_of_version min_version)

    | Section sct, Some(plugin_knd, plugin_name, _),
      SinceVersion min_version ->
      begin
        try
          let plugin_version_current =
            try
              match Data.plugin_version plugin_knd plugin_name data with
                | Some ver -> ver
                | None ->
                  failwithf
                    (f_ "Section %s is only valid for the OASIS \
                         plugin %s since v%s, but no plugin version is \
                         defined in the _oasis file, change '%s' to \
                         '%s (%s)' in your _oasis file.")
                    sct plugin_name (string_of_version min_version)
                    plugin_name
                    plugin_name (string_of_version min_version)
            with Not_found ->
              failwithf
                (f_ "Section %s is only valid when the OASIS plugin %s \
                     is defined.")
                sct plugin_name
          in
          version_is_good ~min_version plugin_version_current
            (f_ "Section %s is only valid for the OASIS plugin %s \
                 since v%s, update your plugin from '%s (%s)' to \
                 '%s (%s)' after checking the plugin's changelog.")
            sct plugin_name (string_of_version min_version)
            plugin_name (string_of_version plugin_version_current)
            plugin_name (string_of_version min_version)
        with Failure msg ->
          Some msg
      end

    | NoOrigin, None, SinceVersion min_version ->
      version_is_good ~min_version data.Data.oasis_version "%s" no_message

    | NoOrigin, Some(plugin_knd, plugin_name, _), SinceVersion min_version ->
      begin
        try
          let plugin_version_current =
            match Data.plugin_version plugin_knd plugin_name data with
              | Some ver -> ver
              | None -> raise Not_found
          in
          version_is_good ~min_version plugin_version_current
            "%s" no_message
        with Not_found ->
          Some no_message
      end


let data_assert t data origin =
  match data_check t data origin with
    | None -> ()
    | Some str -> failwith str


let data_test t data =
  match data_check t data NoOrigin with
    | None -> true
    | Some _ -> false


let package_test t pkg =
  data_test t (Data.of_package pkg)

let create ?plugin name publication description =
  let () =
    if Hashtbl.mem all_features name then
      failwithf "Feature '%s' is already declared." name
  in
  let t =
    {
      name = name;
      plugin = plugin;
      publication = publication;
      description = description;
    }
  in
  Hashtbl.add all_features name t;
  t


let get_stage name =
  try
    (Hashtbl.find all_features name).publication
  with Not_found ->
    failwithf (f_ "Feature %s doesn't exist.") name


let list () =
  Hashtbl.fold (fun _ v acc -> v :: acc) all_features []

(*
 * Real flags.
 *)


let features =
  create "features_fields"
    (since_version "0.4")
    (fun () ->
       s_ "Enable to experiment not yet official features.")


let flag_docs =
  create "flag_docs"
    (since_version "0.3")
    (fun () ->
       s_ "Building docs require '-docs' flag at configure.")


let flag_tests =
  create "flag_tests"
    (since_version "0.3")
    (fun () ->
       s_ "Running tests require '-tests' flag at configure.")


let pack =
  create "pack"
    (since_version "0.3")
    (fun () ->
       s_ "Allow to create packed library.")


let section_object =
  create "section_object" beta
    (fun () ->
       s_ "Implement an object section.")


let dynrun_for_release =
  create "dynrun_for_release" alpha
    (fun () ->
       s_ "Make '-setup-update dynamic' suitable for releasing project.")


let compiled_setup_ml =
  create "compiled_setup_ml" alpha
    (fun () ->
       s_ "It compiles the setup.ml and speed-up actions done with it.")

let disable_oasis_section =
  create "disable_oasis_section" alpha
    (fun () ->
       s_ "Allows the OASIS section comments and digest to be omitted in \
           generated files.")

let no_automatic_syntax =
  create "no_automatic_syntax" alpha
    (fun () ->
       s_ "Disable the automatic inclusion of -syntax camlp4o for packages \
           that matches the internal heuristic (if a dependency ends with \
           a .syntax or is a well known syntax).")
