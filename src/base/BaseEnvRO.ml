
(** Read-only environment
    @author Sylvain Le Gall
  *)

(** Variable type
  *)
type var_t = string

(** Value type
  *)
type val_t = 
    {
      order: int;
      value: string;
    }

(** Read-only environment type
  *)
type env_t = (var_t, val_t) Hashtbl.t

(** Get all variable
  *)
let var_all env =
  List.rev_map
    snd
    (List.sort
       (fun (i1, _) (i2, _) -> i2 - i1)
       (Hashtbl.fold 
          (fun nm vl acc -> (vl.order, nm) :: acc)
          env 
          []))
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
           var_get ~handle_not_found:false var env
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
and var_get ?(handle_not_found=true) name env =
  let vl = 
    try 
      (Hashtbl.find env name).value
    with Not_found when handle_not_found ->
      failwith 
        (Printf.sprintf 
           "No variable %s defined (available: %s)"
           name
           (String.concat ", " (var_all env)))
  in
    var_expand env vl
;;

(** Environment default file 
  *)
let default_filename =
  Filename.concat 
    (Filename.dirname Sys.argv.(0))
    "setup.data"
;;

(** Initialize environment.
  *)
let load ?(allow_empty=false) ?(filename=default_filename) () =
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
        let rec read_file order =
          match Stream.npeek 3 lexer with 
            | [Genlex.Ident nm; Genlex.Kwd "="; Genlex.String value] ->
                Stream.junk lexer; 
                Stream.junk lexer; 
                Stream.junk lexer;
                Hashtbl.add env nm {order = order; value = value};
                read_file (order + 1)
            | [] ->
                ()
            | _ ->
                failwith 
                  (Printf.sprintf 
                     "Malformed data file '%s' line %d"
                     filename !line)
        in
          read_file 0;
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
             "Unable to load environment, the file '%s' doesn't exist."
             filename)
      )
;;
