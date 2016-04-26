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


(** Add fields to _oasis for plugin
    @author Sylvain Le Gall
*)


open OASISTypes


type 'a t = 'a OASISSchema_intern.t


(** [new_field schm plugin_id name value help pivot_data sync]
    Create a field for a plugin. Create a new field for the plugin defined
    by [plugin_id] in the schema [schm]. The field basename is [name], for which
    we will add the name of the plugin and ["X"] before -- to know that this
    field is related to this plugin. [value] defines how to parse/print the
    value. [help] is an helper text for this field. [pivot_data] and [sync]
    defines how to match back the datastructure where the result will be stored
    with the field in _oasis.

    The result of this function is a getter that helps you to get the data
    from the field parsed in the _oasis file. You should use it to generate
    a datastructure (the one that will be used by [sync]).
*)
val new_field:
  ('b t) ->
  OASISPlugin.all_t ->
  name ->
  ?default:'a ->
  ?feature:OASISFeatures.t ->
  'a OASISValues.t ->
  (unit -> string) ->
  'c OASISPlugin.prop -> ('b -> 'c -> 'a) ->
  PropList.Data.t -> 'a


(** Create a conditional field for a plugin. Sees {!new_field} for explanation.
    The extra [default_cond] parameter allows to define a complex default. If
    [default_cond] and [default] are defined together, they are concatened in
    this order.
*)
val new_field_conditional:
  ('b t) ->
  OASISPlugin.all_t ->
  name ->
  ?default_cond:('a OASISExpr.choices) ->
  ?default:'a ->
  ?feature:OASISFeatures.t ->
  'a OASISValues.t ->
  (unit -> string) ->
  'c OASISPlugin.prop -> ('b -> 'c -> 'a OASISExpr.choices) ->
  PropList.Data.t ->
  'a OASISExpr.choices

