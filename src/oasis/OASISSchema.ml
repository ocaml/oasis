
(** Property list and schema checker 
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISAstTypes;;
open CommonGettext;;
open PropList;;

type t =
  (ctxt, string option) PropList.Schema.t
;;

let schema nm : t= 
  Schema.create 
    ~case_insensitive:true 
    nm
;;

let new_field_conditional schm name ?plugin ?default parse help =
  let update old_v v =
    OASISExpr.reduce_choices 
      (old_v @ v)
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
             failwith 
               (Printf.sprintf 
                  (f_ "No context defined for field '%s' when parsing value %S")
                  name 
                  s)
     in
       [real_cond, parse s]
  in

    FieldRO.create 
      ~schema:schm 
      ~name:name
      ~parse:parse 
      ~update:update
      ~default:default
      ~help:help
      plugin
;;

let new_field schm name ?plugin ?default parse help =

  let parse ?context s =
    match context with 
      | Some ctxt ->
          begin
            if ctxt.cond <> None then
              failwith 
                (Printf.sprintf 
                   "Field %s cannot be conditional"
                   name);
            parse s
          end
      | None ->
          (* TODO: this is ugly, try to find a solution without ?context *)
          failwith 
            (Printf.sprintf
              (f_ "No context defined for field '%s' when parsing value %S")
              name 
              s)
  in

    FieldRO.create
      ~schema:schm 
      ~name:name
      ~parse:parse 
      ?default
      ~help:help
      plugin
;;

