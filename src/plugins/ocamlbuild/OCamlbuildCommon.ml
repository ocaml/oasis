
(** Functions common to OCamlbuild build and doc plugin
  *)

open BaseEnv
open BaseStandardVar

let ocamlbuild_clean_ev =
  "ocamlbuild-clean"

(** Fix special arguments depending on environment *)
let fix_args args extra_argv =
  List.flatten
    [
      if (os_type ()) = "Win32" then
        [
          "-classic-display"; 
          "-no-log"; 
          "-no-links";
          "-install-lib-dir"; 
          (Filename.concat (standard_library ()) "ocamlbuild")
        ] 
      else
        [];
  
      if not (bool_of_string (is_native ())) || (os_type ()) = "Win32" then
        [
          "-byte-plugin" 
        ]
      else
        [];
      args;
      Array.to_list extra_argv;
    ]

(** Run 'ocamlbuild -clean' if not already done *)
let run_clean extra_argv =
  let extra_cli =
    String.concat " " (Array.to_list extra_argv)
  in
    (* Run if never called with these args *)
    if not (BaseLog.exists ocamlbuild_clean_ev extra_cli) then
      begin
        BaseExec.run (ocamlbuild ()) (fix_args ["-clean"] extra_argv);
        BaseLog.register ocamlbuild_clean_ev extra_cli
      end

(** Run ocamlbuild, unregister all clean events *)
let run_ocamlbuild args extra_argv =
  BaseExec.run (ocamlbuild ()) (fix_args args extra_argv);
  (* Remove any clean event, we must run it again *)
  List.iter
    (fun (e, d) -> BaseLog.unregister e d)
    (BaseLog.filter [ocamlbuild_clean_ev])

(** Determine real build directory *)
let build_dir extra_argv =
  let rec search_args dir =
    function
      | "-build-dir" :: dir :: tl ->
          search_args dir tl
      | _ :: tl ->
          search_args dir tl
      | [] -> 
          dir
  in
    search_args "_build" (fix_args [] extra_argv)
