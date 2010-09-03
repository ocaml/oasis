
(** Common arguments 
  *)

open MainGettext

let oasis_fn =
  ref "_oasis"

let oasis_fn_specs =
  [
    "-oasis",
    Arg.Set_string oasis_fn,
    s_ "fn _oasis file to use.";
  ]

let ignore_plugins =
  ref false

let ignore_plugins_specs = 
  [
    "-ignore-plugins",
    Arg.Set ignore_plugins,
    s_ " Ignore plugin's field.";
  ]
