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


(*
 * Plugin operation
 *)


let plugin_compare (k1, n1, vo1) (k2, n2, vo2) =
  match compare k1 k2 with
    | 0 ->
      begin
        match OASISUtils.compare_csl n1 n2 with
          | 0 ->
            begin
              match vo1, vo2 with
                | Some v1, Some v2 ->
                  OASISVersion.version_compare v1 v2
                | None, _ | _, None ->
                  0
            end
          | n ->
            n
      end
    | n ->
      n


let plugin_equal plg1 plg2 =
  plugin_compare plg1 plg2 = 0


let plugin_hash (k, n, _) =
  Hashtbl.hash (k, String.lowercase n, None)


(*
 * Plugin properties
 *)


exception Not_set


type 'a setter = plugin_data ref -> 'a -> unit
type 'a getter = plugin_data ref -> 'a
type 'a prop   = 'a setter * 'a getter


let data_create () =
  ref []


let data_new_property ?(purpose: plugin_data_purpose option) plg =
  let knd, _, _ =
    plg
  in
  let prp =
    match purpose, knd with
      | Some p, _ ->
        p
      | _, knd ->
        (knd :> plugin_data_purpose)
  in
  let v =
    ref None
  in
  let set t x =
    t := (plg, prp, (fun () -> v := Some x)) :: !t
  in

  let get t =
    try
      let _, _, f =
        List.find
          (fun (plg', prp', _) ->
             prp = prp' && plugin_equal plg plg')
          !t
      in
      f ();
      match !v with
        | Some x ->
          x
        | None ->
          raise Not_set

    with Not_found ->
      raise Not_set
  in
  (set, get)


(* END EXPORT *)


open OASISGettext
open OASISUtils
open ODNFunc


type modul = string


type ('a, 'b) setup_changes =
  {
    chng_moduls: modul list;
    chng_main: 'a func;
    chng_clean: ('b func) option;
    chng_distclean: ('b func) option;
  }


type context_act =
  {
    ctxt: OASISContext.t;
    update: OASISSetupUpdate.t;
    error: bool;
    files: OASISFileTemplate.templates;
    other_actions: (unit -> unit) list;
  }


type ('a, 'b) section_act =
  context_act ->
  package ->
  (common_section * 'a) ->
  context_act *
    ((package -> (common_section * 'a) -> string array -> 'b),
     (package -> (common_section * 'a) -> string array -> unit)
    ) setup_changes


type package_act =
  context_act ->
  package ->
  context_act *
    ((package -> string array -> unit),
     (package -> string array -> unit)
    ) setup_changes


type 'a t = 'a plugin
type all_t = plugin_kind plugin


module MapPlugin =
  MapExt.Make(
  struct
    type t = plugin_kind plugin
    let compare = plugin_compare
  end)


module SetPlugin =
  SetExt.Make(
  struct
    type t = plugin_kind plugin
    let compare = plugin_compare
  end)


let mem_no_version (knd, nm, _) plugins =
  SetPlugin.fold
    (fun (knd', nm', ver) found ->
       if not found then
         knd = knd' && (OASISUtils.compare_csl nm nm' = 0)
       else
         found)
    plugins false

module HashPlugin =
  Hashtbl.Make
    (struct
      type t = plugin_kind plugin
      let equal = plugin_equal
      let hash  = plugin_hash
    end)


module HashPluginAll =
  Hashtbl.Make
    (struct
      type t = [`All] plugin
      let equal = plugin_equal
      let hash = plugin_hash
    end)


(** Find a plugin with or without version *)
let find_fuzzy tbl ((knd, nm, vo) as id) =
  try
    HashPlugin.find tbl id
  with Not_found ->
    HashPlugin.find tbl (knd, nm, None)


let string_of_plugin (_, nm, vo) =
  match vo with
    | Some v ->
      Printf.sprintf
        "%s (%s)"
        nm
        (OASISVersion.string_of_version v)
    | None ->
      nm


let plugin_of_string knd str =
  match OASISString.split_optional_parentheses str with
    | plg, Some ver ->
      knd, plg, Some (OASISVersion.version_of_string ver)
    | plg, None ->
      knd, plg, None


let plugins_of_string knd str =
  List.map
    (plugin_of_string knd)
    (OASISString.split_comma str)


(* General data for plugin *)


type help =
  {
    help_template:   string list;
    help_order:      int;
  }


let help_all =
  HashPluginAll.create 5


let help_default lst =
  {
    help_template   = lst;
    help_order      = 0;
  }


let register_help (_, nm, vo) hlp =
  HashPluginAll.replace help_all (`All, nm, vo) hlp


let help plg =
  HashPluginAll.find help_all plg


let version_all =
  HashPlugin.create 5


let all =
  version_all


let all_plugins () =
  HashPlugin.fold
    (fun k _ acc -> k :: acc)
    all
    []


(*
 * Quickstart completion
 *)


let qstrt_cmplt_all =
  HashPlugin.create 5


let register_quickstart_completion t f =
  HashPlugin.add qstrt_cmplt_all t f


let quickstart_completion plg =
  try
    find_fuzzy qstrt_cmplt_all plg
  with Not_found ->
    (fun pkg -> pkg)


(*
 * Generators
 *)


let gen_all =
  HashPlugin.create 5


let register_generator_package t (prop_set, _) generator =
  HashPlugin.add gen_all t
    (fun t data -> prop_set t (generator data))


let generator_package plg rplugin_data data =
  try
    let lst =
      HashPlugin.find_all gen_all plg
    in
    List.iter
      (fun gen ->
         gen rplugin_data data)
      lst
  with Not_found ->
    ()


let gen_section =
  HashPlugin.create 5


let register_generator_section knd t (prop_set, _) generator =
  HashPlugin.add gen_section t
    (knd, (fun t data -> prop_set t (generator data)))


let generator_section knd plg rplugin_data data =
  try
    let lst =
      HashPlugin.find_all gen_section plg
    in
    List.iter
      (fun (knd', gen) ->
         if knd = knd' then
           gen rplugin_data data)
      lst
  with Not_found ->
    ()


(** List all plugins *)
let ls knd =
  HashPlugin.fold
    (fun ((knd', _, _) as id) _ lst ->
       if knd = knd' then
         (string_of_plugin id) :: lst
       else
         lst)
    all
    []


let to_plugin t =
  t


module type PLUGINS =
sig
  type data
  type act
  type kind

  type self_t = kind t
  type self_plugin = kind plugin

  val create: self_plugin -> self_t * all_t
  val register_act: self_t -> act -> unit
  val act: self_plugin -> act
  val quickstart_question: unit -> self_plugin quickstart_question
  val value: self_plugin OASISValues.t
end


(* Family of plugins data *)
module type FAMILY =
sig
  type act
  type data
  type kind = private [< plugin_kind]
  val kind_default: kind
  val to_plugin_kind: kind -> plugin_kind
end


module Make (F: FAMILY): PLUGINS with type data = F.data
                                  and type act = F.act
                                  and type kind = F.kind =
struct

  (*
   * Types
   *)

  type data = F.data
  type act = F.act
  type kind = F.kind

  type self_t = kind plugin
  type self_plugin = kind plugin

  module HashPluginGlobal = HashPlugin

  let create plg =
    let ver =
      match plg with
        | _, _, Some v -> v
        | _, _, None ->
          failwithf
            (f_ "Plugin %s is defined without version.")
            (string_of_plugin plg)
    in
    let all_id :> plugin_kind plugin = plg in
    let self_id = plg in
    HashPlugin.add version_all all_id ver;
    self_id, all_id

  module HashPlugin =
    Hashtbl.Make
      (struct
        type t = self_plugin
        let equal = plugin_equal
        let hash  = plugin_hash
      end)

  (* TODO: use a first-class module to handle this case and avoid
   * code duplication
  *)
  (** Find a plugin with or without version *)
  let find_fuzzy' tbl ((knd, nm, vo) as id) =
    try
      HashPlugin.find tbl id
    with Not_found ->
      HashPlugin.find tbl (knd, nm, None)

  (*
   * Act
   *)

  let act_all =
    HashPlugin.create 5

  let register_act t e =
    HashPlugin.add act_all t e

  let act plg =
    try
      find_fuzzy' act_all plg
    with Not_found ->
      failwithf
        (f_ "The plugin %s has no registered action.")
        (string_of_plugin plg)

  (** Parse value *)
  let value =
    let kind_default :> plugin_kind =
      F.kind_default
    in

    let not_found_of_kind =
      function
        | `Configure -> f_ "Unknown configure plugin '%s' (available: %s)"
        | `Build     -> f_ "Unknown build plugin '%s' (available: %s)"
        | `Doc       -> f_ "Unknown doc plugin '%s' (available: %s)"
        | `Test      -> f_ "Unknown test plugin '%s' (available: %s)"
        | `Install   -> f_ "Unknown install plugin '%s' (available: %s)"
        | `Extra     -> f_ "Unknown extra plugin '%s' (available: %s)"
    in

    let parse ~ctxt s =
      let (knd, nm, ver_opt) as plg =
        plugin_of_string kind_default s
      in
      if not ctxt.OASISContext.ignore_plugins then
        begin
          try
            let ver_plg =
              try
                find_fuzzy version_all (plg :> plugin_kind plugin)
              with Not_found ->
                failwithf
                  (not_found_of_kind kind_default)
                  (string_of_plugin plg)
                  (String.concat ", " (ls kind_default))
            in
            match ver_opt with
              | Some ver ->
                if OASISVersion.version_compare ver ver_plg <> 0 then
                  OASISMessage.info ~ctxt
                    (f_ "Plugin %s doesn't match the latest version of \
                         this plugin. Please check plugin's changelog \
                         and upgrade to the latest version %s.")
                    (string_of_plugin plg)
                    (OASISVersion.string_of_version ver_plg)

              | None ->
                OASISMessage.warning ~ctxt
                  (f_ "Plugin %s is defined without version, use \
                       current version at least: %s.")
                  nm
                  (string_of_plugin  (knd, nm, Some ver_plg))

          with Not_found ->
            failwithf "Plugin %s doesn't exist." (string_of_plugin plg)
        end;
      (F.kind_default, nm, ver_opt)
    in
    {
      OASISValues.parse  = parse;
      update             = OASISValues.update_fail;
      print              = string_of_plugin;
    }

  let quickstart_question () =
    let knd :> plugin_kind =
      F.kind_default
    in
    ExclusiveChoices
      (HashPluginGlobal.fold
         (fun (knd', nm, vo) _ lst ->
            if knd' = knd then
              (F.kind_default, nm, vo) :: lst
            else
              lst)
         all
         [])
end


(** Configure plugins
*)
module Configure =
  Make
    (struct
      type act  = package_act
      type data = package
      type kind = [`Configure]
      let kind_default = `Configure
      let to_plugin_kind = function `Configure -> `Configure
    end)


(** Build plugins
*)
module Build =
  Make
    (struct
      type act  = package_act
      type data = package
      type kind = [`Build]
      let kind_default = `Build
      let to_plugin_kind = function `Build -> `Build
    end)


(** Document plugins
*)
module Doc =
  Make
    (struct
      type act  = (doc, unit) section_act
      type data = common_section * doc
      type kind = [`Doc]
      let kind_default = `Doc
      let to_plugin_kind = function `Doc -> `Doc
    end)


(** Test plugins
*)
module Test =
  Make
    (struct
      type act  = (test, float) section_act
      type data = common_section * test
      type kind = [`Test]
      let kind_default = `Test
      let to_plugin_kind = function `Test -> `Test
    end)


(** Install/uninstall plugins
*)
module Install =
  Make
    (struct
      type act  = package_act * package_act
      type data = package
      type kind = [`Install]
      let kind_default = `Install
      let to_plugin_kind = function `Install -> `Install
    end)


(** Extra plugins
*)
module Extra =
  Make
    (struct
      type act  = context_act -> package -> context_act
      type data = package
      type kind = [`Extra]
      let kind_default = `Extra
      let to_plugin_kind = function `Extra -> `Extra
    end)


(** Functions for plugin writer
*)


(** Check that given field name belong to any plugin
*)
let test_field_name nm =
  String.length nm > 0 && (nm.[0] = 'x' || nm.[0] = 'X')


(** Create value for a builtin plugin
*)
let builtin knd nm =
  let builtin_version =
    Some OASISConf.version_short
  in
  knd, nm, builtin_version


(** Add a generated template file
*)
let add_file tmpl ctxt =
  {ctxt with
     files = OASISFileTemplate.add tmpl ctxt.files}


(** Tests for error and set error status
*)
let set_error tst s ctxt =
  if tst then
    begin
      OASISMessage.error ~ctxt:ctxt.ctxt "%s" s;
      {ctxt with error = true}
    end
  else
    ctxt
