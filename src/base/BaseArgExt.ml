
(** Handle command line argument
    @author Sylvain Le Gall
  *)

open BaseEnvironment;;

let tr_arg str =
  let buff =
    Buffer.create (String.length str)
  in
    String.iter 
      (function 
         | '_' | ' ' | '\n' | '\r' | '\t' -> Buffer.add_char buff '-'
         | c -> Buffer.add_char buff c
      )
      str;
    Buffer.contents buff
;;

let enable name hlp default env =
  arg_add
    (* var name *)
    name
    (* default value *)
    (
      if default then
        "true"
      else
        "false"
    )
    (* command line argument *)
    (
      let arg_name =
        tr_arg name
      in
        [
          "--enable-"^arg_name,
          (fun renv -> Arg.Unit (fun () -> renv := var_add name "true" !renv)),
          " Enable "^hlp^(if default then " [default]" else "");

          "--disable-"^arg_name,
          (fun renv -> Arg.Unit (fun () -> renv := var_add name "false" !renv)),
          " Disable "^hlp^(if not default then " [default]" else "");
        ]
    )
    env
;;
 
let wth name hlp default env =
  arg_add
    (* var name *)
    name 
    (* default *)
    default
    (* command line argument *)
    [
      "--with-"^(tr_arg name),
      (fun renv -> Arg.String (fun str -> renv := var_add name str !renv)),
      hlp^" ["^default^"]"
    ]
    env
;;

