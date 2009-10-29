
(** Environment for configure variable
    @author Sylvain Le Gall
  *)

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
      origin:     origin_t;   
      hide:       bool;
      dump:       bool;
      short_desc: string option;
      cli:        cli_handle_t;
      arg_help:   string option;
      group:      string option;
    }
;;

(** Environment type
  *)
type env_t = 
    {
      values:      BaseEnvRO.env_t;
      definitions: (BaseEnvRO.var_t, definition_t) Hashtbl.t;

      mutable last_order:   int;
      mutable print_hidden: bool;
    }
;;

(** See {!BaseEnvRO.var_all}.
  *)
let var_all ?(include_hidden=false) env =
  List.filter
    (fun var ->
       try 
         let def = 
           Hashtbl.find env.definitions var
         in
           not def.hide || include_hidden 
       with Not_found ->
         false)
    (BaseEnvRO.var_all env.values)
;;

(** See {!BaseEnvRO.var_expand}.
  *)
let var_expand env str =
  BaseEnvRO.var_expand env.values str
;;

(** See {!BaseEnvRO.var_get}.
  *)
let var_get ?handle_not_found name env =
  BaseEnvRO.var_get ?handle_not_found name env.values 
;;

(** See {!BaseEnvRO.load}.
  *)
let load ?(filename) ?(allow_empty) () = 
  let ro = 
    BaseEnvRO.load ?filename ?allow_empty ()
  in
  let last_order = 
    Hashtbl.fold
      (fun nm vl last_order -> max vl.BaseEnvRO.order last_order)
      ro
      min_int
  in
    {
      values       = ro;
      definitions  = Hashtbl.create (Hashtbl.length ro);
      last_order   = last_order;
      print_hidden = false;
    }
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

    let current_origin = 
      (* Build value *)
      try
        (* Use directly value if it exists *)
        let vl = 
          Hashtbl.find env.values name
        in
        let current_origin = 
          try 
            (Hashtbl.find env.definitions name).origin
          with Not_found ->
            (* Comes from BaseEnvRO *)
            OFileLoad
        in
        let value = 
          if origin > current_origin then 
            Lazy.force dflt 
          else 
            vl.BaseEnvRO.value
        in
          Hashtbl.replace env.values name {vl with BaseEnvRO.value = value};
          current_origin

      with Not_found ->
        (
          let value, origin =
            (* Use env or default value, depending what is available 
               and their priority
             *)
            let rec try_value exc_opt =
              function
                | (lzy, origin) :: tl ->
                    (
                      try 
                        Lazy.force lzy, origin
                      with 
                        | Not_found -> try_value exc_opt tl
                        (* If unusual exception is raised, remember it *)
                        | e -> try_value (Some e) tl
                    )
                | [] ->
                    (
                      (* No good value found *)
                      match exc_opt with 
                        | Some e -> raise e
                        | None ->
                            failwith 
                              (Printf.sprintf 
                                 "No default value for variable %s"
                                 name)
                    )
            in
              try_value 
                None
                (* Try value in order of origin *)
                (List.sort
                   (fun (_, org1) (_, org2) -> compare org1 org2)
                   [lazy (Sys.getenv name), OGetEnv; dflt, origin])
          in
            env.last_order <- env.last_order + 1;
            Hashtbl.add 
              env.values 
              name 
              {BaseEnvRO.order = env.last_order; value = value};
            origin
        )
    in

    (* Use directly definition if it exists *)
    let def = 
      try 
        Hashtbl.find env.definitions name
      with Not_found ->
        {
          origin     = origin;
          hide       = hide;
          dump       = dump;
          short_desc = short_desc;
          cli        = cli;
          arg_help   = arg_help;
          group      = group;
        }
    in
    let no_default default v def_v =
      if v <> default then
        v
      else
        def_v
    in
    let new_def = 
      {
         origin     = max origin current_origin;
         short_desc = no_default None short_desc def.short_desc;
         cli        = no_default CLINone cli def.cli;
         arg_help   = no_default None arg_help def.arg_help;
         hide       = hide || def.hide;
         dump       = dump && def.dump;
         group      = no_default None group def.group;
      }
    in
      Hashtbl.replace env.definitions name new_def
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
let var_ignore (e : string) = ();;

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

(** Save environment on disk.
  *)
let dump ?(filename=BaseEnvRO.default_filename) env = 
  let chn =
    open_out_bin filename
  in
    Hashtbl.iter
      (fun nm {BaseEnvRO.value = value} -> 
         let dump = 
           try 
             (Hashtbl.find env.definitions nm).dump
           with Not_found ->
             true
         in
           if dump then
             Printf.fprintf chn "%s=%S\n" nm value)
      env.values;
    close_out chn
;;

(** Display environment to user.
  *)
let print env =
  let printable_vars =
    List.map 
      (fun nm -> 
         let vl = 
           Hashtbl.find env.values nm
         in
         let txt = 
           try 
             match (Hashtbl.find env.definitions nm).short_desc with 
               | Some s -> s
               | None -> nm
           with Not_found ->
             nm
         in
           txt, vl.BaseEnvRO.value)
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

           let value = 
             (Hashtbl.find env.values name).BaseEnvRO.value
           in

           let args = 
             match def.cli with 
               | CLINone -> 
                   []
               | CLIAuto -> 
                   [
                     "--"^arg_name,
                     Arg.String var_set,
                     arg_hlp^" "^hlp^" ["^value^"]"
                   ]
               | CLIWith ->
                   [
                     "--with-"^arg_name,
                     Arg.String var_set,
                     arg_hlp^" "^hlp^" ["^value^"]"
                   ]
               | CLIEnable ->
                   [
                     "--enable-"^arg_name,
                     Arg.Unit (fun () -> var_set "true"),
                     " "^hlp^(if value = "true" then " [default]" else "");

                     "--disable-"^arg_name,
                     Arg.Unit (fun () -> var_set "false"),
                     " "^hlp^(if value <> "true" then " [default]" else "");
                   ]
               | CLIUser lst ->
                   lst
           in
             args :: acc)
         env.definitions
         [])
;;

