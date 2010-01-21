
(** Property list and schema checker 
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISAstTypes;;
open CommonGettext;;
open PropList;;

type extra =
  {
    plugin:      string option;
    qckstrt_lvl: string quickstart_level;
    qckstrt_q:   string quickstart_question;
  }
;;

type t =
  (ctxt, extra) PropList.Schema.t
;;

let schema nm: t = 
  Schema.create 
    ~case_insensitive:true 
    nm
;;

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
;;


let new_field_conditional 
      schm 
      name 
      ?plugin 
      ?default 
      ?quickstart_level
      ?quickstart_question
      value 
      help =
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
       [real_cond, value.parse s]
  in

  let print =
    function
      | [EBool true, v] -> value.print v
      | _ ->
          raise OASISValues.Not_printable
  in

    FieldRO.create 
      ~schema:schm 
      ~name:name
      ~parse:parse 
      ~print:print
      ~update:update
      ~default:default
      ~help:help
      (extra
        ?plugin 
        ?quickstart_level
        ?quickstart_question
         value)
;;

let new_field 
      schm 
      name 
      ?plugin 
      ?default 
      ?quickstart_level
      ?quickstart_question
      value 
      help =

  let parse ?context s =
    match context with 
      | Some ctxt ->
          begin
            if ctxt.cond <> None then
              failwith 
                (Printf.sprintf 
                   "Field %s cannot be conditional"
                   name);
            value.parse s
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
      ~print:value.print
      ?default
      ~help:help
      (extra
        ?plugin 
        ?quickstart_level
        ?quickstart_question
         value)
;;

