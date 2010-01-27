
(** Property list 
    @author Sylvain Le Gall
  *)

open CommonGettext

type name_t = string

let no_context f =
  fun ?context s -> f s

exception Not_set of name_t
exception No_printer of name_t
exception Unknown_field of name_t * name_t

let string_of_exception =
  function
    | Not_set nm ->
        Printf.sprintf (f_ "Value %s is not set") nm
    | No_printer nm ->
        Printf.sprintf (f_ "No default printer for value %s") nm
    | Unknown_field (nm, schm) ->
        Printf.sprintf (f_ "Field %s is not defined in schema %s") nm schm
    | e ->
        raise e

module Data =
struct

  type t = 
      (name_t, unit -> unit) Hashtbl.t

  let create () =
    Hashtbl.create 13

(* END EXPORT *)
  let odn_of_t t =
    ODN.APP ("PropList.Data.create", [], [ODN.UNT])
(* START EXPORT *)
end

module Schema = 
struct

  type ('ctxt, 'extra) value_t =
      {
        get:   Data.t -> string;
        set:   Data.t -> ?context:'ctxt  -> string -> unit;
        help:  (unit -> string) option;
        extra: 'extra;
      }

  type ('ctxt, 'extra) t =
      {
        name:      name_t;
        fields:    (name_t, ('ctxt, 'extra) value_t) Hashtbl.t;
        presets:   (name_t, 'ctxt option * string) Hashtbl.t;
        order:     name_t Queue.t;
        name_norm: string -> string;
      }

  let create ?(case_insensitive=false) nm = 
    {
      name      = nm;
      fields    = Hashtbl.create 13;
      presets   = Hashtbl.create 13;
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

    (* If available, set preset values *)
    let update_preset data =
      if Hashtbl.mem t.presets nm then
        begin
          let context, v = 
            Hashtbl.find t.presets nm
          in
            Hashtbl.remove t.presets nm;
            set data ?context v
        end
    in

    (* Set preset value before any other *)
    let set data ?context x =
      update_preset data;
      set data ?context x
    in

    (* Before get, set preset value *)
    let get data =
      update_preset data;
      get data
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
    (find t nm).get 
      data

  let set t data nm ?context x =
    (find t nm).set 
      data 
      ?context 
      x

  let preset t data nm ?context x = 
    Hashtbl.add t.presets nm (context, x)

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

end

module Field =
struct

  type ('ctxt, 'value, 'extra) t =
      {
        set:    Data.t -> 'value -> unit;
        get:    Data.t -> 'value;
        sets:   Data.t -> ?context:'ctxt -> string -> unit;
        gets:   Data.t -> string;
        help:   (unit -> string) option;
        extra:  'extra;
      }

  let new_id = 
    let last_id =
      ref 0
    in
      fun () -> incr last_id; !last_id

  let create ?schema ?name ?parse ?print ?default ?update ?help extra =
    (* Default value container *)
    let v = 
      ref None 
    in

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
        | None -> raise (Not_set nm) 
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
    let set data x = 
      let x = 
        match update with 
          | Some f ->
              begin
                try 
                  f (get data) x
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
            fun ?context s ->
              failwith 
                (Printf.sprintf 
                   (f_ "Cannot parse field '%s' when setting value %S")
                   nm
                   s)
    in

    (* Set data, from string *)
    let sets data ?context s =
      set data (parse ?context s)
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

  let fset data t x = 
    t.set data x

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
