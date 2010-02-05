
(** Various utilities for OASIS.
  *)

open OASISTypes

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

open OASISSchema
open OASISValues
open CommonGettext

let std_field nm comp_dflt schm = 
  let build = 
    new_field_conditional schm "Build"
      ~default:true
      boolean
      (fun () ->
         Printf.sprintf 
           (f_ "Set if the %s should be built. Use with flag.")
           nm)
  in
  let install =
    new_field_conditional schm "Install"
      ~default:true
      boolean
      (fun () ->
         Printf.sprintf
           (f_ "Set if the %s should be distributed.")
           nm)
  in
  let compiled_object =
    new_field schm "CompiledObject"
      ~default:comp_dflt
      compiled_object
      (fun () ->
         Printf.sprintf 
           (f_ "Define the compilation type of %s: byte, native or best")
           nm)
  in
    build, 
    install,
    compiled_object

let build_tools_fields schm =
  (* TODO: this field should look like depends_field, especially be able to 
   * define internal tools to use
   *)
  new_field schm "BuildTools"
    ~default:[]
    (comma_separated string_not_empty)
    (fun () -> s_ "Executables require to compile.")

let depends_field schm = 
  let build_depends = 
    new_field schm "BuildDepends" 
      ~default:[]
      build_depends
      (fun () -> s_ "Dependencies on findlib packages and internal libraries.")
  in
    build_depends, (build_tools_fields schm)

let c_field schm = 
  new_field schm "CSources"
    ~default:[]
    files
    (fun () -> s_ "C source files.")

let data_field schm =
  new_field schm "DataFiles"
    ~default:[]
    data_files
    (fun () -> 
       s_ "Comma separated list of files to be installed for run-time use by \
           the package. Install by default in '$datadir/$pkg_name', you can \
           override using 'fn ($datadir/other_location)'. You can use \
           wildcard '*' but only for filename and followed by a single dot \
           extension: 'dir/*.html' is valid but 'dir/*' and 'dir/*.tar.gz' are \
           not valid.")
