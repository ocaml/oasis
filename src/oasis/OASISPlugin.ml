
(** Functions for plugin writer
  *)

(** Check that given field name belong to any plugin
  *)
let test_field_name nm = 
  String.length nm > 0 && (nm.[0] = 'x' || nm.[0] = 'X')
;;

(** Create field name derived from a plugin 
  *)
let make_field_name plugin nm = 
  "X"^plugin^nm
;;

(** See {!OASIS.new_field}
  *)
let new_field schm plugin nm ?default parse =
  OASISSchema.new_field 
    schm 
    (make_field_name plugin nm) 
    ?default
    ~plugin:plugin
    parse
;;

(** See {!OASIS.new_field_conditional}
  *)
let new_field_conditional schm plugin nm ?default parse =
  OASISSchema.new_field_conditional 
    schm 
    (make_field_name plugin nm) 
    ?default 
    ~plugin:plugin
    parse 
;;

