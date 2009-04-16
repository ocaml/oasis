
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
      default: env -> string;
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

(** Type for transforming env
  *)
type fun_env = env -> env
;;

(** Apply in turn function to modify environment
  *)
let chain lst env =
  List.fold_left
    (fun env f -> f env)
    env
    lst
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

(** Retrieve the value of a variabe
  *)
let var_get ?(mandatory=false) ?(error_extra_message="") name env =
  try 
    MapVar.find name env.value
  with Not_found ->
    (
      try
        (MapVar.find name env.defined).default env
      with Not_found ->
        (
          if mandatory then
            failwith
              (Printf.sprintf 
                 "Variable %S not defined %S"
                 name
                 error_extra_message)
          else
            raise Not_found
        )
    )
;;

(** Try to get a variable value from environment and if it fails
  * compute it and store it in environment.
  *)
let cache ?(hide) name dflt env =
  try 
    (var_get name env), env
  with Not_found ->
    (
      let v, env =
        dflt env
      in
        v,
        var_set 
          name 
          v 
          (var_define ?hide name (fun _ -> v) env)
    )
;;

(** Get all variable
  *)
let var_all ?(include_hidden=false) env =
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
               (name, lazy value) :: varset_lst, vardflt_mp
             else
               varset_lst, vardflt_mp
         with Not_found ->
           (* No definition we display it *)
           (name, lazy value) :: varset_lst, vardflt_mp)
      env.value
      ([], env.defined)
  in

    (* Combine set var with defaulted var *)
    MapVar.fold
      (fun name def lst ->
         if include_hidden || (not def.hidden) then
           (name, lazy (def.default env)) :: lst
         else
           lst)
      vardflt_mp
      varset_lst
;;

(** Expand variable that can be found in string. [multi] control
  * wether the variable will be expanded until there is nothing to 
  * expand anymore. Variable follow definition of variable for 
  * {!Buffer.substitute}. To escape '$', you must use '$_'.
  *)
let rec var_expand ?(deep=true) ?(error_extra_message="") str env =
  let all_vars () =
    String.concat ", " (List.map fst (var_all env))
  in
  let subst f str =
    let buffer_replaced =
      Buffer.create ((String.length str) * 2)
    in
      Buffer.add_substitute 
        buffer_replaced
        f
        str;
      Buffer.contents buffer_replaced
  in
  let nstr =
    subst 
      (fun varname ->
         if varname = "_" then
           "$_"
         else
           var_get 
             ~mandatory:true
             ~error_extra_message:(Printf.sprintf
                                     "in %S %s; available variables: %s"
                                     str
                                     error_extra_message
                                     (all_vars ()))
             varname 
             env
      )
      str
  in
    if deep && nstr <> str then
      (
        var_expand 
          ~deep:deep
          ~error_extra_message:error_extra_message 
          nstr
          env
      )
    else
      (
        (* Proceed with the final replacement of $_ by $ *)
        subst
          (function 
             | "_" ->
                 "$"
             | str ->
                 failwith 
                   ("Unknown replacement "^str^" "^error_extra_message)
          )
          nstr
      )
;;

(** Save environment on disk.
  *)
let dump fn env = 
  let chn =
    open_out_bin fn
  in
    List.iter 
      (fun (nm,vl) -> 
         Printf.fprintf chn "%s = %S\n" nm (Lazy.force vl))
      (var_all ~include_hidden:true env);
    close_out chn
;;

(** Initialize environment.
  *)
let init fn pkg_name pkg_version = 
  let default =
    chain
      [
        var_define "pkg_name" (fun _ -> pkg_name);
        var_define "pkg_version" (fun _ -> pkg_version);
      ]
      {
        defined      = MapVar.empty;
        value        = MapVar.empty;
        print_hidden = false;
      }
  in
    if Sys.file_exists fn then
      (
        let chn =
          open_in_bin fn
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
                     fn !line)
        in
        let mp =
          read_file default.value
        in
          close_in chn;
          {default with value = mp}
      )
    else
      (
        default
      )
;;

(** Display environment to user.
  *)
let print env =
  let printable_vars =
    var_all ~include_hidden:env.print_hidden env
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
       Printf.printf "%s: %s %s\n" name (dot_pad name) (Lazy.force value))
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

