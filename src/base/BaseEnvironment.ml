
(** Environment for configure variable
    @author Sylvain Le Gall
  *)

module Msg    = BaseMessage;;
module MapVar = Map.Make(String);;

type var = string

(** Variable type
  *)
type definition =
    {
      value:   string;
      hidden:  bool;
    }
;;

(** Environment type
  *)
type env = (var, definition) Hashtbl.t
;;


(** Get all variable
  *)
let var_all ?(include_hidden=false) env =
  Hashtbl.fold 
    (fun var def acc ->
       if not def.hidden || include_hidden then
         var :: acc
       else
         acc)
    env 
    []
;;

(** Set a variable 
  *)
let var_set ?(hide=false) name dflt env =
  let def = 
    try 
      (* Use directly definition if it exists *)
      Hashtbl.find env name
    with Not_found ->
      (
        (* Build one definition using either value from 
         * env or default definition
         *)
        let from_env =
          try 
            Sys.getenv name 
          with Not_found ->
            Lazy.force dflt
        in
          {value = from_env; hidden = hide}
      )
  in
    Hashtbl.replace 
      env 
      name 
      {value = def.value; hidden = hide};
;;

(** Expand variable that can be found in string. Variable follow definition of
  * variable for {!Buffer.add_substitute}.
  *)
let rec var_expand env str =
  let buff =
    Buffer.create ((String.length str) * 2)
  in
    Buffer.add_substitute 
      buff
      (fun var -> 
         try 
           var_get var env
         with Not_found ->
           failwith 
             (Printf.sprintf 
                "No variable %s defined when trying to expand %S \
                 (available: %s)"
                var 
                str 
                (String.concat ", " (var_all env))))
      str;
    Buffer.contents buff

(** Get variable 
  *)
and var_get name env =
  let vl = 
    (Hashtbl.find env name).value
  in
    var_expand env vl
;;

(** Add a variable to environment and return its value. [hide] allow to store
    a variable that will be hidden to user (not printed).
  *)
let var_define ?hide name dflt env =
  var_set ?hide name dflt env;
  var_get name env 
;;

(** Well-typed ignore for var_define 
  *)
let var_ignore (e : string) =
  ()
;;

(** Protect a variable content, to avoid expansion
  *)
let var_protect vl = 
  let buff = 
    Buffer.create (String.length vl)
  in
    String.iter
      (function 
         | '$' -> Buffer.add_string buff "\\$"
         | c   -> Buffer.add_char   buff c)
      vl;
    Buffer.contents buff
;;

(** Environment file 
  *)
let filename =
  Filename.concat 
    (Filename.dirname Sys.argv.(0))
    "setup.data"
;;

(** Save environment on disk.
  *)
let dump env = 
  let chn =
    open_out_bin filename
  in
    Hashtbl.iter
      (fun nm def -> Printf.fprintf chn "%s=%S\n" nm def.value)
      env;
    close_out chn
;;

(** Initialize environment.
  *)
let load ?(allow_empty=false) () = 
  let env =
    Hashtbl.create 13
  in
    if Sys.file_exists filename then
      (
        let chn =
          open_in_bin filename
        in
        let st =
          Stream.of_channel chn
        in
        let line =
          ref 1
        in
        let st_line = 
          Stream.from
            (fun _ ->
               try
                 match Stream.next st with 
                   | '\n' -> incr line; Some '\n'
                   | c -> Some c
               with Stream.Failure -> None)
        in
        let lexer = 
          Genlex.make_lexer ["="] st_line
        in
        let rec read_file () =
          match Stream.npeek 3 lexer with 
            | [Genlex.Ident nm; Genlex.Kwd "="; Genlex.String vl] ->
                Stream.junk lexer; 
                Stream.junk lexer; 
                Stream.junk lexer;
                var_set nm (lazy vl) env;
                read_file ()
            | [] ->
                ()
            | _ ->
                failwith 
                  (Printf.sprintf 
                     "Malformed data file '%s' line %d"
                     filename !line)
        in
          read_file ();
          close_in chn;
          env
      )
    else if allow_empty then
      (
        env
      )
    else
      (
        failwith 
          (Printf.sprintf 
             "Unable to load environment file '%s', maybe run '%s -configure'"
             filename
             Sys.argv.(0))
      )
;;

(** Display environment to user.
  *)
let print env =
  let printable_vars =
    Hashtbl.fold
      (fun nm def acc ->
         if def.hidden then
           acc
         else
           (nm, def.value) :: acc)
      env
      []
  in
  let max_length = 
    List.fold_left
      (fun mx (name, _) -> max (String.length name) mx)
      0
      printable_vars
  in
  let dot_pad str =
    String.make ((max_length - (String.length str)) + 3) '.'
  in

  print_newline ();
  print_endline "Configuration: ";
  print_newline ();
  List.iter 
    (fun (name,value) -> 
       Printf.printf "%s: %s %s\n" name (dot_pad name) value)
    printable_vars;
  Printf.printf "%!";
  print_newline ();
;;

(** Default command line arguments 
  *)
let args env =
  [
    "--override",
     Arg.Tuple
       (
         let rvr = ref ""
         in
         let rvl = ref ""
         in
           [
             Arg.Set_string rvr;
             Arg.Set_string rvl;
             Arg.Unit (fun () -> var_set !rvr (lazy !rvl) env)
           ]
       ),
    "var+val  Override any configuration variable";

    (* TODO: reactivate 
    "--print-hidden",
    Arg.Unit (fun () -> renv := {!renv with print_hidden = true}),
    " Print even non-printable variable (debug)";
     *)
  ]
;;

