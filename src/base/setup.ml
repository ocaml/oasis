open BuildSys

(* takes BuildSys' env, generates an environment suitable for
 * Spec.package_of_string *)
let flags_of_env env =
  (* TODO: fill with platform-specific flags *)
  Spec.empty_environment ()

let read_file f =
  let ch = open_in f in
  let b = Buffer.create 80 in
  let buf = String.create 4096 in
  let rec loop () = match input ch buf 0 4096 with
          0 -> close_in ch; Buffer.contents b
        | n -> Buffer.add_substring b buf 0 n; loop ()
  in loop ()

let write_if_nonexistent ~dst s =
  if not (Sys.file_exists dst) then
    let ch = open_out dst in
      try output_string ch s; close_out ch with e -> close_out ch; raise e

let create_mllib lib = match lib.Spec.lib_buildable, lib.Spec.lib_modules with
    true, Some mods ->
      let dst = lib.Spec.lib_path ^ ".mllib" in
        write_if_nonexistent ~dst (String.concat "\n" mods)
  | _ -> ()

let simple pkgname =
  let spec = read_file (pkgname ^ ".auto") in
  let parsed = Spec.parse Spec.schema spec in
  let empty_flags = Spec.empty_environment () in
  let targets, env =
    Action.configure
      (Spec.get_field_value "Name" parsed empty_flags)
      (Spec.get_field_value "Version" parsed empty_flags)
      (* command-line args *)
      [
        BuildArg.base;
      ]
      (* checks *)
      [
        Check.ocamlbuild_base
      ]
      (* .in files *)
      [
      ] in
  let pkg = Spec.package_of_string (flags_of_env env) spec in
    (* process targets using the info from the Spec.package type *)
    write_if_nonexistent ~dst:"myocamlbuild.ml"
      (Spec.myocamlbuild (List.map fst pkg.Spec.build_depends));
    List.iter create_mllib pkg.Spec.libraries;
    (* TODO: invoke ocamlbuild to build targets. Use BuildSys.Action? *)
    ()

