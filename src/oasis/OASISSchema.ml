
(** Property list and schema checker 
    @author Sylvain Le Gall
  *)

open OASISTypes;;
open OASISAstTypes;;

module NameMap = Map.Make(String);;

exception MissingField of name list;;

exception UnknownField of name;;

type name = string;;

type field =
    {
      set:  ctxt -> (unit -> unit) -> string -> (unit -> unit);
      get:  unit -> unit;                     
    }
;;

type schema = (name, field) Hashtbl.t;;

type writer = 
    {
      defined:       (name, field) Hashtbl.t;
      mutable extra: string NameMap.t;
    }
;;

let schema () = 
  Hashtbl.create 13
;;

let new_field_low schm name set default getmod v =
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
    Hashtbl.replace schm lname 
      {
        get  = (fun () -> v := Some (get_default ())); 
        set  = set;
      };
    get
;;

let new_field_conditional schm name ?default parse =
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
         match ctxt.OASISAstTypes.cond with 
           | Some e -> e
           | None -> ETrue
       in
         v := Some ((real_cond, parse ctxt str) :: frmr_values))
  in

  let default =
    match default with 
      | Some x ->
          [ETrue, x]
      | None ->
          []
  in

    new_field_low schm name set (Some default) List.rev v 
;;


let new_field schm name ?default parse =
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

    new_field_low schm name set default (fun x -> x) v
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
        if String.length lname > 0 && lname.[0] = 'x' then
          (* This is an extra field *)
          wrtr.extra <- NameMap.add
                          lname 
                          str
                          wrtr.extra
        else
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
    defined = Hashtbl.copy schm;
    extra   = NameMap.empty;
  }
;;

let extra wrtr =
  NameMap.fold
    (fun k e acc -> (k, e) :: acc)
    wrtr.extra
    []
;;

