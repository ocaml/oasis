
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
