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
  type t 

  val register: t -> unit

  val new_field: 
    OASISSchema.t -> 
    name -> 
    ?default:'a -> 
    'a OASISValues.t -> 
    (unit -> string) -> 
    PropList.Data.t -> 
    'a

  val new_field_conditional: 
    OASISSchema.t -> 
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
  val version: string
  val help: string list
  val help_extra_vars: (string * (unit -> string)) list
  val help_order: int
end

module type PLUGINS =
sig
  type t
  module Make: functor (PI : PLUGIN_ID_TYPE) -> PLUGIN_UTILS_TYPE with type t = t
  val ls :        unit ->      name list
  val find :      name * 'a -> t
  val value :     (name * OASISVersion.t option) OASISValues.t
end

module MapPlugin = Map.Make (
struct 
  type t = string 

  let compare nm1 nm2 = 
    String.compare 
      (String.lowercase nm1)
      (String.lowercase nm2)
end)


(* General data for plugin *)
let help_all =
  ref MapPlugin.empty

module Make =
  functor (F:(sig
                (* Family of plugins data *)
                type t
                val family_string: string
                val not_found_fmt: 
                  unit -> (string -> string -> string, unit, string) format
              end)) ->
  struct 

    open OASISValues

    type t = F.t

    let all : ((F.t * string) MapPlugin.t) ref = 
      ref MapPlugin.empty

    module Make (PI: PLUGIN_ID_TYPE) : PLUGIN_UTILS_TYPE with type t = t =
    struct

       type t = F.t

       let () = 
         help_all :=
         MapPlugin.add
           PI.name (PI.version, PI.help_order, PI.help, PI.help_extra_vars) 
           !help_all

       let register e = 
         all := MapPlugin.add PI.name (e, PI.version) !all

       (** Create field name derived from a plugin 
         *)
       let make_field_name nm = 
         "X"^PI.name^nm

       (** See {!OASIS.new_field}
         *)
       let new_field schema nm ?default parse =
         OASISSchema.new_field 
           schema
           (make_field_name nm) 
           ?default
           (* TODO: use an id here *)
           ~plugin:PI.name
           parse

       (** See {!OASIS.new_field_conditional}
         *)
       let new_field_conditional schema nm ?default parse =
         OASISSchema.new_field_conditional 
           schema
           (make_field_name nm) 
           ?default 
           (* TODO: use an id here *)
           ~plugin:PI.name
           parse 
     end

    (** List all plugins *)
    let ls () = 
      List.rev
        (MapPlugin.fold 
           (fun k (_, v) lst -> (k^" ("^v^")") :: lst)
           !all
           [])

    (** Find a specific plugin and return all informations *)
    let find_full (nm, ver_opt) =
      try
        MapPlugin.find nm !all
      with Not_found ->
        failwithf2
          (F.not_found_fmt ())
          nm 
          (String.concat ", " (ls ()))

    (** Find a specific plugin generator *)
    let find k = 
      fst (find_full k)

    (** Parse value for plugin *)
    let value =
      let base = 
        with_optional_parentheses
          string_not_empty
          OASISVersion.value
      in
      let parse ~ctxt s =
        let (nm, ver_opt) as k = 
          (* TODO: check for plugin availability *)
          base.OASISValues.parse ~ctxt s
        in
          begin
            match ver_opt with 
              | Some v ->
                  ()
              | None ->
                  begin
                    let (_, plg_ver) = 
                      find_full k
                    in
                      OASISMessage.warning ~ctxt
                        (f_ "Plugin %s is defined without version, use current \
                             version at least: %s.")
                        nm 
                        (base.OASISValues.print (nm, Some (OASISVersion.version_of_string plg_ver)))
                  end
          end;
          k
      in
        {
          parse  = parse;
          update = OASISValues.update_fail;
          print  = (fun e -> base.print e)
        }
  end

(** Configure plugins
  *)
module Configure = 
  Make
    (struct
       type t = package_act
       let family_string = "configure"
       let not_found_fmt =  
         (fun () -> f_ "Unknown configure plugin '%s' (available: %s)")
     end)

(** Build plugins 
  *)
module Build =
  Make
    (struct
       type t = package_act
       let family_string = "build"
       let not_found_fmt =
         (fun () -> f_ "Unknown build plugin '%s' (available: %s)")
     end)

(** Document plugins 
  *)
module Doc =
  Make
    (struct 
       type t = (doc, unit) section_act
       let family_string = "doc"
       let not_found_fmt =
         (fun () -> f_ "Unknown doc plugin '%s' (available: %s)")
     end)

(** Test plugins
  *)
module Test =
  Make
    (struct
       type t = (test, float) section_act
       let family_string = "test"
       let not_found_fmt =
         (fun () -> f_ "Unknown test plugin '%s' (available: %s)")
     end)

(** Install/uninstall plugins
  *)
module Install =
  Make
    (struct
       type t = package_act * package_act
       let family_string = "install"
       let not_found_fmt =
         (fun () -> f_ "Unknown install plugin '%s' (available: %s)")
     end)

(** Extra plugins
  *)
module Extra =
  Make
    (struct
       type t = context_act -> package -> context_act
       let family_string = "extra"
       let not_found_fmt =
         (fun () -> f_ "Unknown extra plugin '%s' (available: %s)")
     end)

(** Functions for plugin writer
  *)

(** Check that given field name belong to any plugin
  *)
let test_field_name nm = 
  String.length nm > 0 && (nm.[0] = 'x' || nm.[0] = 'X')

(** Create value for a builtin plugin 
  *)
let builtin nm =
  let builtin_version =
    Some (OASISVersion.version_of_string OASISConf.version)
  in
    nm, builtin_version

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

