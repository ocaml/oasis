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


open OASISGettext


type name = string


exception Not_set of name * string option
exception No_printer of name
exception Unknown_field of name * name


let () =
  Printexc.register_printer
    (function
      | Not_set (nm, Some rsn) ->
        Some
          (Printf.sprintf (f_ "Field '%s' is not set: %s") nm rsn)
      | Not_set (nm, None) ->
        Some
          (Printf.sprintf (f_ "Field '%s' is not set") nm)
      | No_printer nm ->
        Some
          (Printf.sprintf (f_ "No default printer for value %s") nm)
      | Unknown_field (nm, schm) ->
        Some
          (Printf.sprintf
             (f_ "Field %s is not defined in schema %s") nm schm)
      | _ ->
        None)


module Data =
struct
  type t =
    (name, unit -> unit) Hashtbl.t

  let create () =
    Hashtbl.create 13

  let clear t =
    Hashtbl.clear t


  (* END EXPORT *)
  let elements t =
    let rlst = ref [] in
    Hashtbl.iter
      (fun nm _ -> rlst := nm :: !rlst)
      t;
    !rlst
  (* START EXPORT *)
end


module Schema =
struct
  type ('ctxt, 'extra) value =
    {
      get:   Data.t -> string;
      set:   Data.t -> ?context:'ctxt -> string -> unit;
      help:  (unit -> string) option;
      extra: 'extra;
    }

  type ('ctxt, 'extra) t =
    {
      name:      name;
      fields:    (name, ('ctxt, 'extra) value) Hashtbl.t;
      order:     name Queue.t;
      name_norm: string -> string;
    }

  let create ?(case_insensitive=false) nm =
    {
      name      = nm;
      fields    = Hashtbl.create 13;
      order     = Queue.create ();
      name_norm =
        (if case_insensitive then
           String.lowercase
         else
           fun s -> s);
    }

  let add t nm set get extra help =
    let key =
      t.name_norm nm
    in

    if Hashtbl.mem t.fields key then
      failwith
        (Printf.sprintf
           (f_ "Field '%s' is already defined in schema '%s'")
           nm t.name);
    Hashtbl.add
      t.fields
      key
      {
        set   = set;
        get   = get;
        help  = help;
        extra = extra;
      };
    Queue.add nm t.order

  let mem t nm =
    Hashtbl.mem t.fields nm

  let find t nm =
    try
      Hashtbl.find t.fields (t.name_norm nm)
    with Not_found ->
      raise (Unknown_field (nm, t.name))

  let get t data nm =
    (find t nm).get data

  let set t data nm ?context x =
    (find t nm).set
      data
      ?context
      x

  let fold f acc t =
    Queue.fold
      (fun acc k ->
         let v =
           find t k
         in
         f acc k v.extra v.help)
      acc
      t.order

  let iter f t =
    fold
      (fun () -> f)
      ()
      t

  let name t =
    t.name
end


module Field =
struct
  type ('ctxt, 'value, 'extra) t =
    {
      set:    Data.t -> ?context:'ctxt -> 'value -> unit;
      get:    Data.t -> 'value;
      sets:   Data.t -> ?context:'ctxt -> string -> unit;
      gets:   Data.t -> string;
      help:   (unit -> string) option;
      extra:  'extra;
    }

  let new_id =
    let last_id = ref 0 in
    fun () -> incr last_id; !last_id

  let create ?schema ?name ?parse ?print ?default ?update ?help extra =
    (* Default value container *)
    let v = ref None in

    (* If name is not given, create unique one *)
    let nm =
      match name with
        | Some s -> s
        | None -> Printf.sprintf "_anon_%d" (new_id ())
    in

    (* Last chance to get a value: the default *)
    let default () =
      match default with
        | Some d -> d
        | None -> raise (Not_set (nm, Some (s_ "no default value")))
    in

    (* Get data *)
    let get data =
      (* Get value *)
      try
        (Hashtbl.find data nm) ();
        match !v with
          | Some x -> x
          | None -> default ()
      with Not_found ->
        default ()
    in

    (* Set data *)
    let set data ?context x =
      let x =
        match update with
          | Some f ->
            begin
              try
                f ?context (get data) x
              with Not_set _ ->
                x
            end
          | None ->
            x
      in
      Hashtbl.replace
        data
        nm
        (fun () -> v := Some x)
    in

    (* Parse string value, if possible *)
    let parse =
      match parse with
        | Some f ->
          f
        | None ->
          fun ?context:_ s ->
            failwith
              (Printf.sprintf
                 (f_ "Cannot parse field '%s' when setting value %S")
                 nm
                 s)
    in

    (* Set data, from string *)
    let sets data ?context s =
      set ?context data (parse ?context s)
    in

    (* Output value as string, if possible *)
    let print =
      match print with
        | Some f ->
          f
        | None ->
          fun _ -> raise (No_printer nm)
    in

    (* Get data, as a string *)
    let gets data =
      print (get data)
    in

    begin
      match schema with
        | Some t ->
          Schema.add t nm sets gets extra help
        | None ->
          ()
    end;

    {
      set   = set;
      get   = get;
      sets  = sets;
      gets  = gets;
      help  = help;
      extra = extra;
    }

  let fset data t ?context x =
    t.set data ?context x

  let fget data t =
    t.get data

  let fsets data t ?context s =
    t.sets data ?context s

  let fgets data t =
    t.gets data
end


module FieldRO =
struct
  let create ?schema ?name ?parse ?print ?default ?update ?help extra =
    let fld =
      Field.create ?schema ?name ?parse ?print ?default ?update ?help extra
    in
    fun data -> Field.fget data fld
end
