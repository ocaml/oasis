
let () =
  Arg.parse
    [
      "-load",
      Arg.String Dynlink.loadfile,
      "fn load a file"
    ]
    ignore
    ""


let () =
  if !Entry_point.counter <> 2 then
    failwith
      (Printf.sprintf
         "number of loaded modules: got %d, want 2"
         !Entry_point.counter)
