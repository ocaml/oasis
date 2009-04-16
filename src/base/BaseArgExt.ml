
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

let enable name hlp default renv =
  let arg_name =
    tr_arg name
  in
    renv := var_define 
              name 
              (fun _ -> if default then "true" else "false")
              !renv;
    [
      "--enable-"^arg_name,
      Arg.Unit (fun () -> renv := var_set name "true" !renv),
      " Enable "^hlp^(if default then " [default]" else "");

      "--disable-"^arg_name,
      Arg.Unit (fun () -> renv := var_set name "false" !renv),
      " Disable "^hlp^(if not default then " [default]" else "");
    ]
;;
 
let wth name hlp default renv =
  renv := var_define name default !renv;
  [
    "--with-"^(tr_arg name),
    Arg.String (fun str -> renv := var_set name str !renv),
    hlp^" ["^(var_get name !renv)^"]"
  ]
;;

let merge lst renv =
  List.flatten
    (List.map 
       (fun fargs -> fargs renv)
       lst)
;;

let parse argv fargs env =
    (* Simulate command line for Arg *)
    let current =
      ref 0
    in

    let renv = 
      ref env
    in
    let args =
      fargs renv
    in
      (
        try
          Arg.parse_argv
            ~current:current
            (Array.concat [[|"none"|]; argv])
            (Arg.align args)
            (fun str -> 
               failwith 
                 ("Don't know what to do with arguments: '"^str^"'"))
            "configure options:"
        with Arg.Help txt | Arg.Bad txt ->
          BaseMessage.error txt
      );
      !renv
;;
