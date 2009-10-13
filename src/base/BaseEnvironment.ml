
(** Environment for configure variable
    @author Sylvain Le Gall
  *)

module Msg    = BaseMessage;;
module MapVar = Map.Make(String);;

type var_t = string

(** Origin of the variable, if a variable has been already set
    with a higher origin, it won't be set again
  *)
type origin_t = 
  | ODefault     (** Default computed value *)
  | OGetEnv      (** Extracted from environment, using Sys.getenv *)
  | OFileLoad    (** From loading file setup.data *)
  | OCommandLine (** Set on command line *)
;;

(** Command line handling for variable 
  *)
type cli_handle_t =
  (** No command line argument *)
  | CLINone
  (** Build using variable name and help text *)
  | CLIAuto
   (** Use prefix --with- *)
  | CLIWith
  (** Use --enable/--disable *)
  | CLIEnable
  (** Fully define the command line arguments *)
  | CLIUser of (Arg.key * Arg.spec * Arg.doc) list
;;

(** Variable type
  *)
type definition_t =
    {
      value:      string;
      origin:     origin_t;   
      hide:       bool;
      dump:       bool;
      short_desc: string option;
      cli:        cli_handle_t;
      arg_help:   string option;
      group:      string option;
      order:      int;
    }
;;

(** Environment type
  *)
type env = 
    {
      vars:   (var_t, definition_t) Hashtbl.t;

      mutable last_order:   int;
      mutable print_hidden: bool;
    }
;;

(** Get all variable
  *)
let var_all ?(include_hidden=false) env =
  List.rev_map
    snd
    (List.sort
       (fun (i1, _) (i2, _) -> i2 - i1)
       (Hashtbl.fold 
          (fun var def acc ->
             if not def.hide || include_hidden then
               (def.order, var) :: acc
             else
               acc)
          env.vars 
          []))
;;

(** Set a variable 
  *)
let var_set 
      ?(hide=false) 
      ?(dump=true) 
      ?short_desc
      ?(cli=CLINone)
      ?arg_help
      ?group 
      origin 
      name
      dflt
      env =
    try 
      (* Use directly definition if it exists *)
      let def = 
        Hashtbl.find env.vars name
      in
      let no_default default v def_v =
        if v <> default then
          v
        else
          def_v
      in
      let new_def = 
        {
           value = 
             (if origin > def.origin then 
                Lazy.force dflt 
              else 
                def.value);
           origin     = max origin def.origin;
           short_desc = no_default None short_desc def.short_desc;
           cli        = no_default CLINone cli def.cli;
           arg_help   = no_default None arg_help def.arg_help;
           hide       = hide || def.hide;
           dump       = dump && def.dump;
           group      = no_default None group def.group;
           order      = def.order;
        }
      in
        if def <> new_def then
          Hashtbl.replace env.vars name new_def
        else
          ()
    with Not_found ->
      (
        (* Build one definition using either value from 
         * env or default definition
         *)
        let raised_exn = 
          ref None
        in
        let lzy, origin =
          (* Use env or default value, depending what is available 
             and their priority
           *)
          try 
            List.find
              (fun (lzy, _) -> 
                 try 
                   ignore (Lazy.force lzy); 
                   true 
                 with 
                   | Not_found -> 
                       false
                   | e ->
                       (* Remember unusual exception *)
                       raised_exn := Some e;
                       false)
              (List.sort
                 (fun (_, org1) (_, org2) -> compare org1 org2)
                 [lazy (Sys.getenv name), OGetEnv; dflt, origin])
          with Not_found ->
            (
              match !raised_exn with 
                | Some e ->
                    raise e
                | None ->
                    failwith 
                      (Printf.sprintf 
                         "No default value for variable %s"
                         name)
            )
        in
          env.last_order <- env.last_order + 1;
          Hashtbl.add
            env.vars
            name
            {
              value      = Lazy.force lzy;
              origin     = origin;
              hide       = hide;
              dump       = dump;
              short_desc = short_desc;
              cli        = cli;
              arg_help   = arg_help;
              group      = group;
              order      = env.last_order;
            }
      )
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
      (Hashtbl.find env.vars name).value
    with Not_found when handle_not_found ->
      failwith 
        (Printf.sprintf 
           "No variable %s defined (available: %s)"
           name
           (String.concat ", " (var_all env)))
  in
    var_expand env vl
;;

(** Add a variable to environment and return its value. [hide] allow to store
    a variable that will be hidden to user (not printed).
  *)
let var_define ?hide ?dump ?short_desc ?cli ?arg_help ?group name dflt env =
  var_set ?hide ?dump ?short_desc ?cli ?arg_help ?group ODefault name dflt env;
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
      (fun nm def -> 
         if def.dump then
           Printf.fprintf chn "%s=%S\n" nm def.value)
      env.vars;
    close_out chn
;;

(** Initialize environment.
  *)
let load ?(allow_empty=false) () = 
  let env =
    {
      vars         = Hashtbl.create 13;
      print_hidden = false;
      last_order   = 0;
    }
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
                var_set OFileLoad nm (lazy vl) env;
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
    List.map 
      (fun var -> 
         let def = 
           Hashtbl.find env.vars var
         in
         let txt =
           match def.short_desc with 
             | Some s -> s
             | None   -> var
         in
           txt, def.value)
      (var_all ~include_hidden:env.print_hidden env)
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
  in
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
               Arg.Unit (fun () -> var_set OCommandLine !rvr (lazy !rvl) env)
             ]
         ),
      "var+val  Override any configuration variable.";

      "--print-hidden",
      Arg.Unit (fun () -> env.print_hidden <- true),
      " Print even non-printable variable. (debug)";
    ]
    @
    List.flatten 
      (Hashtbl.fold
        (fun name def acc ->
           let var_set s = 
             var_set OCommandLine name (lazy s) env
           in

           let arg_name = 
             tr_arg name
           in

           let hlp =
             match def.short_desc with 
               | Some txt -> txt
               | None -> ""
           in

           let arg_hlp =
             match def.arg_help with 
               | Some s -> s
               | None   -> "str"
           in

           let args = 
             match def.cli with 
               | CLINone -> 
                   []
               | CLIAuto -> 
                   [
                     "--"^arg_name,
                     Arg.String var_set,
                     arg_hlp^" "^hlp^" ["^def.value^"]"
                   ]
               | CLIWith ->
                   [
                     "--with-"^arg_name,
                     Arg.String var_set,
                     arg_hlp^" "^hlp^" ["^def.value^"]"
                   ]
               | CLIEnable ->
                   [
                     "--enable-"^arg_name,
                     Arg.Unit (fun () -> var_set "true"),
                     " "^hlp^(if def.value = "true" then " [default]" else "");

                     "--disable-"^arg_name,
                     Arg.Unit (fun () -> var_set "false"),
                     " "^hlp^(if def.value <> "true" then " [default]" else "");
                   ]
               | CLIUser lst ->
                   lst
           in
             args :: acc)
         env.vars
         [])
;;

