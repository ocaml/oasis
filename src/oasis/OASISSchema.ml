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

(** Property list and schema checker 
    @author Sylvain Le Gall
  *)

open OASISTypes
open OASISAstTypes
open OASISGettext
open OASISUtils
open OASISValues
open OASISExpr
open PropList

module Sync = 
struct

  type 'a t = ('a -> PropList.Data.t) ref

  let create () = 
    ref (fun _ -> PropList.Data.create ())

  let add t schm value nm sync = 
    let fake_context = 
      {
        OASISAstTypes.cond = None;
        append             = false;
        valid_flags        = [];
        ctxt               = !OASISContext.default;
      }
    in

    let prev_f = 
      !t
    in

    let new_f = 
      fun pkg -> 
        let data = prev_f pkg in
        let () = 
          try 
            (* TODO: we should just restore the value 
             * wether or not it is printable should not
             * be important 
             *)
            PropList.Schema.set 
              schm 
              data 
              (* TODO: really need to kill this ~context *)
              ~context:fake_context 
              nm 
              (value.print (sync pkg))
          with Not_printable ->
            ()
        in
          data
    in
      t := new_f
end

type kind = 
  | DefinePlugin 
  | DefinePlugins
  | FieldFromPlugin of string
  | StandardField 

type extra =
  {
    kind:        kind;
    qckstrt_lvl: string quickstart_level;
    qckstrt_q:   unit -> string quickstart_question;
  }

type 'a t =
    {
      schm: (ctxt, extra) PropList.Schema.t;
      sync: 'a Sync.t;
    }


let schema nm = 
  {
    schm = Schema.create ~case_insensitive:true nm;
    sync = Sync.create ()
  }

(** Define extra data contained in the schema
  *)
let extra 
      ?(kind=StandardField)
      ?(quickstart_level=Expert) 
      ?(quickstart_question=(fun () -> Field))
      value =
          
  let qckstrt_lvl =
    match quickstart_level with
      | NoChoice v -> NoChoice (value.print v)
      | Beginner | Intermediate | Expert as l ->
          l
  in

  let qckstrt_q () =
    match quickstart_question () with
      | Choices lst ->
          Choices (List.map value.print lst)
      | ExclusiveChoices lst ->
          ExclusiveChoices (List.map value.print lst)
      | Field | Text as q ->
          q
  in

    {
      kind        = kind;
      qckstrt_lvl = qckstrt_lvl;
      qckstrt_q   = qckstrt_q;
    }


(** Create  a conditional field
  *)
let new_field_conditional 
      t
      name 
      ?plugin 
      ?default 
      ?quickstart_level
      ?quickstart_question
      value 
      help
      sync =
  let update ?context old_choices new_choices =
    let choices =
      match context with 
        | Some {append = true} ->
            (* Append in conditional context means a cartesian product between
               the choices and the (None :: Some + appends). We then combine
               condition with && and values with value.update.

               WARNING: quadratic expansion in space
             *)
            let all_appends = 
              None :: (List.map (fun v -> Some v) new_choices)
            in
              List.flatten
                (List.map
                   (fun (cond, choice) ->
                      List.map
                        (function
                           | None ->
                               cond, choice
                           | Some (new_cond, append) ->
                               EAnd(cond, new_cond), 
                               value.update choice append)
                        all_appends)
                   old_choices)
        | _ ->
            old_choices @ new_choices
    in
      OASISExpr.reduce_choices choices
  in

  let default = 
    match default with 
      | Some x ->
          Some [EBool true, x]
      | None ->
          None
  in

  let parse ?context s = 
     match context with 
       | Some ctxt ->
           begin
             let real_cond =
               match ctxt.cond with 
                 | Some e -> e
                 | None -> EBool true
             in
               [real_cond, value.parse ~ctxt:ctxt.ctxt s]
           end
       | None ->
           (* TODO: this is ugly, try to find a solution without ?context *)
           failwithf2
             (f_ "No context defined for field '%s' when parsing value %S")
             name 
             s
  in

  let trivial_value = 
    function
      | [] -> 
          raise (PropList.Not_set(name, None))
      | [EBool true, v] -> 
          v
      | _ ->
          raise OASISValues.Not_printable
  in

  let print lst =
    value.print (trivial_value lst)
  in

  let kind =
    match plugin with
      | Some plg -> Some (FieldFromPlugin plg)
      | None -> None
  in

  let sync pkg = 
    (* TODO: this prevent to synchronize complex
     * conditional value 
     *)
    trivial_value (sync pkg)
  in


    Sync.add t.sync t.schm value name sync;
    FieldRO.create 
      ~schema:t.schm 
      ~name
      ~parse 
      ~print
      ~update
      ?default
      ~help
      (extra
        ?kind 
        ?quickstart_level
        ?quickstart_question
         value)

(** Default parser and updater for new_field and new_field_plugin
  *)
let default_parse_update name value = 

  let update ?context old_v v =
    match context with 
      | Some {append = true} ->
          value.update old_v v
      | _ ->
          v
  in

  let parse ?context s =
    match context with 
      | Some ctxt ->
          begin
            if ctxt.cond <> None then
              failwithf1 (f_ "Field '%s' cannot be conditional") name;
            value.parse ~ctxt:ctxt.ctxt s
          end
      | None ->
          (* TODO: this is ugly, try to find a solution without ?context *)
          failwithf2 
            (f_ "No context defined for field '%s' when parsing value %S")
            name s
  in

    update, parse
  

(** Create a simple field
  *)
let new_field 
      t
      name 
      ?plugin 
      ?default 
      ?quickstart_level
      ?quickstart_question
      value 
      help
      sync =

  let update, parse = 
    default_parse_update name value
  in
  let kind =
    match plugin with
      | Some plg -> Some (FieldFromPlugin plg)
      | None -> None
  in

    Sync.add t.sync t.schm value name sync;
    FieldRO.create
      ~schema:t.schm 
      ~name
      ~parse 
      ~print:value.print
      ~update
      ?default
      ~help
      (extra
        ?kind 
        ?quickstart_level
        ?quickstart_question
         value)

(** Create a field that enables a plugin 
  *)
let new_field_plugin 
      t
      name
      ?default 
      ?quickstart_level
      ?quickstart_question
      value 
      help
      sync =

  let update, parse =
    default_parse_update name value
  in
    Sync.add t.sync t.schm value name sync;
    FieldRO.create
      ~schema:t.schm 
      ~name
      ~parse 
      ~print:value.print
      ~update
      ?default
      ~help
      (extra
        ~kind:DefinePlugin 
        ?quickstart_level
        ?quickstart_question
         value)

(** Create a field that enables some plugins
  *)
let new_field_plugins
      t
      name
      ?default 
      ?quickstart_level
      ?quickstart_question
      value 
      help
      sync =

  let values = 
    comma_separated value 
  in
  let update, parse =
    default_parse_update name values
  in
    Sync.add t.sync t.schm values name sync;
    FieldRO.create
      ~schema:t.schm 
      ~name
      ~parse 
      ~print:values.print
      ~update
      ?default
      ~help
      (extra
        ~kind:DefinePlugins
        ?quickstart_level
        ?quickstart_question
        value)

let to_proplist t = 
  let f = !(t.sync) in
    fun e -> t.schm, f e
