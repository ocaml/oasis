let test () =
  let argv_len = Array.length Sys.argv in
    if argv_len < 2 then
      true
    else
      try
        let len = int_of_string Sys.argv.(1) in
          len = pred argv_len
      with
        | _ -> false
