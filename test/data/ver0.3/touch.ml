let () = 
  Arg.parse 
    []
    (fun fn -> close_out (open_out fn))
    "foo"
