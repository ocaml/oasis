
(** Various utilities for OASIS.
  *)

module MapString = Map.Make(String)

(** Build a MapString with an association list 
  *)
let map_string_of_assoc assoc =
  List.fold_left
    (fun acc (k, v) -> MapString.add k v acc)
    MapString.empty
    assoc

(** Set for String 
  *)
module SetString = Set.Make(String)

(** Add a list to a SetString
  *)
let set_string_add_list st lst =
  List.fold_left 
    (fun acc e -> SetString.add e acc)
    st
    lst

(** Build a set out of list 
  *)
let set_string_of_list =
  set_string_add_list
    SetString.empty

(** Split a string, separator not included
  *)
let split sep str =
  let str_len =
    String.length str
  in
  let rec split_aux acc pos =
    if pos < str_len then
      (
        let pos_sep = 
          try
            String.index_from str pos sep
          with Not_found ->
            str_len
        in
        let part = 
          String.sub str pos (pos_sep - pos) 
        in
        let acc = 
          part :: acc
        in
          if pos_sep >= str_len then
            (
              (* Nothing more in the string *)
              List.rev acc
            )
          else if pos_sep = (str_len - 1) then
            (
              (* String end with a separator *)
              List.rev ("" :: acc)
            )
          else
            (
              split_aux acc (pos_sep + 1)
            )
      )
    else
      (
        List.rev acc
      )
  in
    split_aux [] 0

(* END EXPORT *)
