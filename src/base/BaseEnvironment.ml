
(** Environment for configure variable
    @author Sylvain Le Gall
  *)

module Msg    = BaseMessage;;
module MapVar = Map.Make(String);;
module SetVar = Set.Make(String);;

(** Variable type
  *)
type definition =
    {
      default: env -> string * env;
      hidden:  bool;
    }

(** Environment type
  *)
and env = 
    {
      defined:      definition MapVar.t;
      value:        string MapVar.t;
      print_hidden: bool;
    }
;;

(** Add a variable to environment. [hide] allow to store
  * a variable that will be hidden to user (not printed).
  *)
let var_define ?(hide=false) name dflt env =
  if not (MapVar.mem name env.defined) then
    {
      env with 
          defined = MapVar.add 
                      name 
                      {default = dflt; hidden = hide} 
                      env.defined;
    }
  else
    failwith 
      (Printf.sprintf
         "Variable %S is already defined"
         name)
;;

(** Set value of a variable 
  *)
let var_set name value env =
  if MapVar.mem name env.defined then
    {
      env with
          value = MapVar.add name value env.value;     
    }
  else
    failwith 
      (Printf.sprintf 
         "Cannot set variable %S because it is not defined" 
         name)
;;

(** Retrieve the value of a variable
  *)
let var_get ?(mandatory=false) name env =
  try 
    MapVar.find name env.value, env
  with Not_found ->
    (
      try
        let def = 
          MapVar.find name env.defined
        in
        let dflt, env = 
          def.default env
        in
          if not def.hidden then 
            (
              Msg.checking name;
              Msg.result dflt
            );
          dflt, var_set name dflt env
      with Not_found ->
        (
          if mandatory then
            failwith
              (Printf.sprintf 
                 "Variable %S not defined"
                 name)
          else
            raise Not_found
        )
    )
;;

(** Try to get a variable value from environment and if it fails
  * compute it and store it in environment.
  *)
let var_cache ?(hide) name dflt env =
  try 
    var_get name env
  with Not_found ->
    var_get name (var_define ?hide name dflt env)
;;

(** Get all variable
  *)
let var_all ?(include_hidden=false) ~include_unset env =
  (* Extract variable that have been set and return
   * a map containing defined value that have not 
   * been set yet.
   *)
  let varset_lst, vardflt_mp =
    MapVar.fold
      (fun name value (varset_lst, vardflt_mp) ->
         (* We look if each var is hidden or not *)
         try
           let def =
             MapVar.find name vardflt_mp
           in
           let vardflt_mp =
             (* We won't need this var in the future *)
             MapVar.remove name vardflt_mp
           in
             if include_hidden || (not def.hidden) then
               name :: varset_lst, vardflt_mp
             else
               varset_lst, vardflt_mp
         with Not_found ->
           (* No definition we display it *)
           name :: varset_lst, vardflt_mp)
      env.value
      ([], env.defined)
  in

    if include_unset then
      (
        (* Combine set var with defaulted var *)
        MapVar.fold
          (fun name def lst ->
             if include_hidden || (not def.hidden) then
               name :: lst
             else
               lst)
          vardflt_mp
          varset_lst
      )
    else
      (
        varset_lst
      )
;;

(** Expand variable that can be found in string. Variable follow definition of
  * variable for {!Buffer.add_substitute}.
  *)
let rec var_expand renv str =
  let all_vars () =
    String.concat ", " (var_all ~include_unset:true !renv)
  in
  let buff =
    Buffer.create ((String.length str) * 2)
  in
    Buffer.add_substitute 
      buff
      (fun var -> 
         try 
           let value, env = 
             var_get var !renv
           in
             renv := env;
             var_expand renv value
         with Not_found ->
           failwith 
             (Printf.sprintf 
                "No variable %s defined when trying to expand %S \
                 (available: %s)"
                str var (all_vars ()))) 
      str;
    Buffer.contents buff
;;

(** Retrieve a variable value, without taking into account
    environment modification (even warning about it). Variable
    must be defined.
  *)
let get env nm = 
  let vl, env' =
    var_get ~mandatory:true nm env
  in
    if env <> env' then
      Msg.warning 
        (Printf.sprintf 
           "Evaluation of variable '%s' lead to unexpected \
            environment modification"
           nm);
    vl
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
  let _env: env =
    List.fold_left 
      (fun env nm -> 
         let vl, env = 
           var_get nm env 
         in
           Printf.fprintf chn "%s = %S\n" nm vl;
           env)
      env
      (var_all ~include_hidden:true ~include_unset:false env)
  in
    close_out chn
;;

(** Initialize environment.
  *)
let load ?(allow_empty=false) () = 
  let default =
      {
        defined      = MapVar.empty;
        value        = MapVar.empty;
        print_hidden = false;
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
        let rec read_file mp =
          match Stream.npeek 3 lexer with 
            | [Genlex.Ident nm; Genlex.Kwd "="; Genlex.String vl] ->
                Stream.junk lexer; 
                Stream.junk lexer; 
                Stream.junk lexer;
                read_file (MapVar.add nm vl mp)
            | [] ->
                mp
            | _ ->
                failwith 
                  (Printf.sprintf 
                     "Malformed data file '%s' line %d"
                     filename !line)
        in
        let mp =
          read_file default.value
        in
          close_in chn;
          {default with value = mp}
      )
    else if allow_empty then
      (
        default
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
  let printable_vars, env =
    List.fold_left
      (fun (acc, env) nm ->
         let vl, env =
           var_get nm env
         in
           ((nm, vl) ::  acc), env)
      ([], env)
      (var_all ~include_hidden:env.print_hidden ~include_unset:false env)
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
  print_newline ();
;;

(** Equality between two environment 
  *)
let equal env1 env2 =
  (MapVar.equal (=) env1.value env2.value)
;;

(** Default command line arguments 
  *)
let args renv =
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
             Arg.Unit (fun () -> renv := var_set !rvr !rvl !renv)
           ]
       ),
    "var+val  Override any configuration variable";

    "--print-hidden",
    Arg.Unit (fun () -> renv := {!renv with print_hidden = true}),
    " Print even non-printable variable (debug)";
  ]
;;

