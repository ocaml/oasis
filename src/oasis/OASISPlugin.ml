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


open OASISTypes
open OASISGettext
open OASISUtils
open ODNFunc
open ExtString

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

module type PLUGIN_UTILS_TYPE =
sig
  type act 
  type data

  val register_act: act -> unit
  val register_quickstart_completion: (package -> package) -> unit

  val new_field: 
    ('b OASISSchema.t) -> 
    name -> 
    ?default:'a -> 
    'a OASISValues.t -> 
    (unit -> string) -> 
    PropList.Data.t -> 
    'a

  val new_field_conditional: 
    ('b OASISSchema.t) -> 
    name -> 
    ?default:'a -> 
    'a OASISValues.t -> 
    (unit -> string) -> 
    PropList.Data.t -> 
    'a OASISExpr.choices

end

module type PLUGIN_ID_TYPE = 
sig 
  val name: string
  val version: OASISVersion.t
  val help: string list
  val help_extra_vars: (string * (unit -> string)) list
  val help_order: int
end

module type PLUGINS =
sig
  type act
  type data
  type kind

  module Make: functor (PI : PLUGIN_ID_TYPE) -> 
    PLUGIN_UTILS_TYPE with 
    type act = act 
    and type data = data

  val ls : unit -> name list

  val find : kind plugin -> act

  val quickstart_question: unit -> (kind plugin) quickstart_question

  val value : (kind plugin) OASISValues.t

  val quickstart_completion: kind plugin -> package -> package
end

let family_of_kind =
  function
    | `Configure -> "configure"
    | `Build     -> "build"
    | `Doc       -> "doc"
    | `Test      -> "test"
    | `Install   -> "install"
    | `Extra     -> "extra"

let not_found_of_kind =
  function
    | `Configure -> f_ "Unknown configure plugin '%s' (available: %s)"
    | `Build     -> f_ "Unknown build plugin '%s' (available: %s)"
    | `Doc       -> f_ "Unknown doc plugin '%s' (available: %s)"
    | `Test      -> f_ "Unknown test plugin '%s' (available: %s)"
    | `Install   -> f_ "Unknown install plugin '%s' (available: %s)"
    | `Extra     -> f_ "Unknown extra plugin '%s' (available: %s)"

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

module MapPlugin = Map.Make (
struct 
  type t = plugin_kind plugin
  let compare = plugin_compare 
end)

module SetPlugin = Set.Make (
struct 
  type t = plugin_kind plugin
  let compare = plugin_compare
end) 

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
  match split_optional_parentheses str with 
    | plg, Some ver ->
        knd, plg, Some (OASISVersion.version_of_string ver)
    | plg, None ->
        knd, plg, None

let plugins_of_string knd str =
  List.map 
    (plugin_of_string knd) 
    (split_comma str)

(* General data for plugin *)

let help_all = 
  ref MapPlugin.empty 

let help () =
  !help_all

module Make =
  functor (F:(sig
                (* Family of plugins data *)
                type act
                type data
                type kind 
                val kind_default: kind
                val to_plugin_kind: kind -> plugin_kind
              end)) ->
  struct 

    open OASISValues

    type act = F.act
    type data = F.data
    type kind = F.kind

    module MapPluginGlobal = MapPlugin

    module MapPlugin = 
      Map.Make 
        (struct
           type t = F.kind plugin
           let compare = plugin_compare 
         end)

    let act_all : ((F.act * OASISVersion.t) MapPlugin.t) ref = 
      ref MapPlugin.empty

    let qstrt_cmplt_all : ((package -> package) MapPlugin.t) ref =
      ref MapPlugin.empty

    module Make (PI: PLUGIN_ID_TYPE) : PLUGIN_UTILS_TYPE with 
      type act = act and type data = data =
    struct

       type act = F.act
       type data = F.data

       let id = F.kind_default, PI.name, Some PI.version
       let plugin_id = F.to_plugin_kind F.kind_default, PI.name, Some PI.version

       let () = 
         help_all :=
         MapPluginGlobal.add
           plugin_id
           (PI.version, PI.help_order, PI.help, PI.help_extra_vars) 
           !help_all

       let register_act e = 
         act_all := MapPlugin.add id (e, PI.version) !act_all


       let register_quickstart_completion f = 
         qstrt_cmplt_all := MapPlugin.add id f !qstrt_cmplt_all

       (** Create field name derived from a plugin 
         *)
       let make_field_name nm = 
         "X"^PI.name^nm

       (** See {!OASIS.new_field}
         *)
       let new_field schema nm ?default parse hlp =
         OASISSchema.new_field 
           schema
           (make_field_name nm) 
           ?default
           ~plugin:plugin_id
           parse
           hlp 
           (* TODO *)
           (fun _ -> 
              raise Not_printable)

       (** See {!OASIS.new_field_conditional}
         *)
       let new_field_conditional schema nm ?default parse hlp =
         OASISSchema.new_field_conditional 
           schema
           (make_field_name nm) 
           ?default 
           ~plugin:plugin_id
           parse 
           hlp
           (* TODO *)
           (fun _ -> 
              raise Not_printable)
     end

    (** List all plugins *)
    let ls () = 
      List.rev
        (MapPlugin.fold 
           (fun ((knd, _, _) as id) _ lst -> 
              (string_of_plugin id) :: lst)
           !act_all
           [])

    (** Find a specific plugin and return all informations *)
    let find_full ((knd, nm, vo) as id) =
      try
        MapPlugin.find id !act_all
      with Not_found ->
        begin
          try 
            MapPlugin.find (knd, nm, None) !act_all

          with Not_found ->
            failwithf2
              (not_found_of_kind (F.to_plugin_kind F.kind_default))
              (string_of_plugin id)
              (String.concat ", " (ls ()))
        end

    (** Find a specific plugin generator *)
    let find k = 
      fst (find_full k)

    (** Parse value for plugin *)
    let value =
      let parse ~ctxt s =
        let (knd, nm, ver_opt) as plg = 
          plugin_of_string F.kind_default s
        in
          (* TODO: ignore these checks if ignore plugin is set *)
          begin
            try 
              let (_, ver_plg) = 
                find_full plg
              in
                match ver_opt with 
                  | Some ver ->
                      if OASISVersion.version_compare ver ver_plg <> 0 then
                        OASISMessage.warning ~ctxt
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
              failwithf1 "Plugin %s doesn't exist." (string_of_plugin plg)
          end;
          plg
      in
        {
          parse  = parse;
          update = OASISValues.update_fail;
          print  = string_of_plugin;
        }

    let quickstart_question () = 
      ExclusiveChoices
        (MapPlugin.fold
           (fun e _ lst -> e :: lst)
           !act_all
           [])

    let quickstart_completion plg = 
      try
        MapPlugin.find plg !qstrt_cmplt_all
      with Not_found ->
        (fun pkg -> pkg)

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
    Some OASISConf.version
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
      OASISMessage.error ~ctxt:ctxt.ctxt ~exit:false "%s" s;
      {ctxt with error = true}
    end
  else
    ctxt



