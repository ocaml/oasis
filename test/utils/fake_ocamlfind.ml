
(* Fake ocamlfind program. *)
let () =
  let real_ocamlfind =
    try
      Sys.getenv "REAL_OCAMLFIND"
    with Not_found ->
      failwith "You need to set REAL_OCAMLFIND env variable."
  in
  let args = Sys.argv in
    args.(0) <- real_ocamlfind;
    Unix.execv real_ocamlfind args
