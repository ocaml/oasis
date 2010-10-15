
open OASISTypes

type 'a t = 'a OASISSchema_intern.t

(** Create field name derived from a plugin 
  *)
let make_field_name (_, nm', _) nm = 
  "X"^nm'^nm

(** Synchronize between plugin datastructure and PropList.Data.t
  *)
let sync_proxy schema t nm (_, get) sync ds =  
  try 
    let t' = 
      (* Extract plugin data from datastructure *)
      get (ref (schema.OASISSchema_intern.plugin ds))
    in
      sync ds t'
  with e ->
    Printf.eprintf 
      "Field %S: %s\n"
      (make_field_name t nm)
      (OASISMessage.string_of_exception e);
    raise OASISValues.Not_printable

let new_field schema t nm ?default parse hlp pivot_data sync =
  let plugin =
    OASISPlugin.to_plugin t
  in
    OASISSchema_intern.new_field 
      schema
      (make_field_name plugin nm) 
      ?default
      ~plugin
      parse
      hlp 
      (sync_proxy schema plugin nm pivot_data sync)

let new_field_conditional schema t nm ?default parse hlp pivot_data sync =
  let plugin =
    OASISPlugin.to_plugin t
  in
    OASISSchema_intern.new_field_conditional 
      schema
      (make_field_name plugin nm) 
      ?default 
      ~plugin
      parse 
      hlp
      (sync_proxy schema plugin nm pivot_data sync)

