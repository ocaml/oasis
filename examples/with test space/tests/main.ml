let () =
  if Argv.test () then (
    print_endline "Ok!";
    exit 0
  )
  else (
    prerr_endline "Error!";
    exit 1
  )
