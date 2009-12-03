
(** Property list and schema checker 
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISAstTypes;;
open CommonGettext;;

type field =
    {
      set:    ctxt -> (unit -> unit) -> string -> (unit -> unit);
      get:    unit -> unit;                     
      help:   string;
      plugin: string option;
    }
;;

type schema_t = 
    {
      name:          string;
      fields:        (name, field) Hashtbl.t;
      mutable order: name list;
    }
;;

type schema_reader_t = 
    {
      defined:       (name, field) Hashtbl.t;
    }
;;

let schema nm = 
  {
    name   = nm;
    fields = Hashtbl.create 13;
    order  = [];
  }
;;

let new_field_low schm name set plugin default getmod v help =
  let get_default () = 
    match default with
      | Some x ->
          x
      | None ->
          raise (MissingField [name])
  in

  let lname =
    String.lowercase name
  in

  let get wrtr =
    (Hashtbl.find wrtr.defined lname).get ();
    match !v with
      | Some x -> 
          getmod x
      | None -> 
          raise (MissingField [name])
  in
    Hashtbl.replace schm.fields lname 
      {
        get    = (fun () -> v := Some (get_default ())); 
        set    = set;
        help   = help;
        plugin = plugin;
      };
    schm.order <- name :: schm.order;
    get
;;

let new_field_conditional schm name ?plugin ?default parse =
  let v =
    ref None
  in

  let set ctxt get str =
    (fun () ->
       let frmr_values =
         get ();
         match !v with 
           | Some lst -> lst
           | None -> []
       in
       let real_cond =
         match ctxt.cond with 
           | Some e -> e
           | None -> EBool true
       in
         v := Some ((real_cond, parse ctxt str) :: frmr_values))
  in

  let default =
    match default with 
      | Some x ->
          [EBool true, x]
      | None ->
          []
  in

  let post_process lst =
    OASISExpr.reduce_choices (List.rev lst)
  in

    new_field_low schm name set plugin (Some default) post_process v 
;;


let new_field schm name ?plugin ?default parse =
  let v = 
    ref None 
  in

  let set ctxt get str =
    (fun () -> 
       if ctxt.OASISAstTypes.cond <> None then
         failwith 
           (Printf.sprintf 
              "Field %s cannot be conditional"
              name)
       else
         v := Some (parse ctxt str))
  in

    new_field_low schm name set plugin default (fun x -> x) v
;;

let set_field wrtr name ctxt str =
  let lname =
    String.lowercase name
  in
    try
      let fld =
        Hashtbl.find 
          wrtr.defined
          lname
      in
        Hashtbl.replace 
          wrtr.defined
          lname 
          {fld with get = fld.set ctxt fld.get str}
    with Not_found ->
      (
        raise (UnknownField name)
      )
;;

let check wrtr =
  let msgfld =
    Hashtbl.fold
      (fun nm fld msgfld -> 
         try
           fld.get ();
           msgfld
         with 
           | MissingField [hd] ->
               hd :: msgfld 
           | MissingField lst ->
               lst @ msgfld)
      wrtr.defined
      []
  in
    if msgfld <> [] then
      raise (MissingField msgfld)
;;

let writer schm =
  {
    defined = Hashtbl.copy schm.fields;
  }
;;

