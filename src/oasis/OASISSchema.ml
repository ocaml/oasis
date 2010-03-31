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
open PropList

type extra =
  {
    plugin:      string option;
    qckstrt_lvl: string quickstart_level;
    qckstrt_q:   string quickstart_question;
  }

type t =
  (ctxt, extra) PropList.Schema.t

let schema nm: t = 
  Schema.create 
    ~case_insensitive:true 
    nm

let extra 
      ?plugin 
      ?(quickstart_level=Expert) 
      ?(quickstart_question=Field)
      value =
          
  let qckstrt_lvl =
    match quickstart_level with
      | NoChoice v -> NoChoice (value.print v)
      | Beginner | Intermediate | Expert as l ->
          l
  in

  let qckstrt_q =
    match quickstart_question with
      | Choices lst ->
          Choices (List.map value.print lst)
      | ExclusiveChoices lst ->
          ExclusiveChoices (List.map value.print lst)
      | YesNo | Field | Text as q ->
          q
  in

    {
      plugin      = plugin;
      qckstrt_lvl = qckstrt_lvl;
      qckstrt_q   = qckstrt_q;
    }


let new_field_conditional 
      schm 
      name 
      ?plugin 
      ?default 
      ?quickstart_level
      ?quickstart_question
      value 
      help =
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
          [EBool true, x]
      | None ->
          []
  in

  let parse ?context s = 
     let real_cond =
       match context with 
         | Some ctxt ->
             begin
               match ctxt.cond with 
                 | Some e -> e
                 | None -> EBool true
             end
         | None ->
             (* TODO: this is ugly, try to find a solution without ?context *)
             failwithf2
               (f_ "No context defined for field '%s' when parsing value %S")
               name 
               s
     in
       [real_cond, value.parse s]
  in

  let print =
    function
      | [] -> 
          raise (PropList.Not_set(name, None))
      | [EBool true, v] -> 
          value.print v
      | _ ->
          raise OASISValues.Not_printable
  in

    FieldRO.create 
      ~schema:schm 
      ~name
      ~parse 
      ~print
      ~update
      ~default
      ~help
      (extra
        ?plugin 
        ?quickstart_level
        ?quickstart_question
         value)

let new_field 
      schm 
      name 
      ?plugin 
      ?default 
      ?quickstart_level
      ?quickstart_question
      value 
      help =

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
            value.parse s
          end
      | None ->
          (* TODO: this is ugly, try to find a solution without ?context *)
          failwithf2 
            (f_ "No context defined for field '%s' when parsing value %S")
            name s
  in

    FieldRO.create
      ~schema:schm 
      ~name
      ~parse 
      ~print:value.print
      ~update
      ?default
      ~help
      (extra
        ?plugin 
        ?quickstart_level
        ?quickstart_question
         value)

let new_field_phantom ?plugin ?name () =
  Field.create ?name (extra ?plugin OASISValues.blackbox)
