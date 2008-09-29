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

let simple pkgname =
  let specdata = read_file (pkgname ^ ".auto") in
  let parsed = Spec.parse Spec.spec specdata in
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
  let pkg = Spec.package_of_string (flags_of_env env) specdata in
    (* process targets using the info from the Spec.package type *)
    ignore pkg

