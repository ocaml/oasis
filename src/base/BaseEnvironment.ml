
(** Environment for configure variable
    @author Sylvain Le Gall
  *)

module Msg    = BaseMessage;;
module MapVar = Map.Make(String);;
module SetFn  = Set.Make(String);;

(** Variable type
  *)
type var =
    {
      value:     string;
      no_export: bool;
    }
;;

(** Environment part that is not saved between invocation
  *)
type env_no_dump = 
    {
      args:            (Arg.key * (env ref -> Arg.spec) * Arg.doc) list;
      fn:              string;
      print_no_export: bool;
      print_conf_done: bool;
    }

(** Environment type
  *)
and env = 
    {
      vars:            var MapVar.t;
      temporary_files: SetFn.t;
      no_dump :        env_no_dump;
    }
;;

(** Type for transforming env
  *)
type fun_env = env -> env
;;

(** Apply in turn function to modify environment
  *)
let chain (lst: (env -> env) list) (env: env) =
  List.fold_left
    (fun env f -> f env)
    env
    lst
;;

(** Add a variable to environment. [no_export] allow to store
  * a variable that will be hidden to user (not printed).
  *)
let var_add ?(no_export=false) name vl env =
  let nvars =
    MapVar.add 
      name 
      {value = vl; no_export = no_export} 
      env.vars
  in
    {
      env with 
          vars  = nvars;
    }
;;

(** Retrieve a variable value from environment
  *)
let var_get ?(mandatory=false) ?(error_extra_message="") name env =
  try 
    (MapVar.find name env.vars).value
  with Not_found ->
    (
      if mandatory then
        Msg.error ("variable '"^name^"' is not defined "^error_extra_message)
      else
        raise Not_found
    )
;;

(** Try to get a variable value from environment and if it fails
  * compute it and store it in environment.
  *)
let cache ?(no_export=false) name f env =
  try
    var_get name env, env
  with Not_found ->
    (
      let vl, nenv =
        f env
      in
      let nnenv =
        var_add ~no_export:no_export name vl nenv
      in
        vl, nnenv
    )
;;

(** Expand variable that can be found in string. [multi] control
  * wether the variable will be expanded until there is nothing to 
  * expand anymore. Variable follow definition of variable for 
  * {!Buffer.substitute}. To escape '$', you must use '$_'.
  *)
let rec var_expand ?(multi=true) ?(error_extra_message="") str env =
  let all_vars =
    String.concat ", "
      (MapVar.fold 
         (fun k v acc -> if v.no_export then acc else k :: acc)
         env.vars 
         []
      )
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
             ~error_extra_message:("in '"^str^"' "^error_extra_message^
                                   "; available variables: "^all_vars)
             varname 
             env
      )
      str
  in
    if multi && nstr <> str then
      (
        var_expand 
          ~multi:multi 
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

(** Add a command line argument 
  *)
let arg_add name default args env =
  let nenv =
    {
      env with
          no_dump = 
            {
              env.no_dump with 
                  args = args @ env.no_dump.args
            }
    }
  in
    snd (cache name (fun env -> default, env) nenv)
;;

(** Get command line argument 
  *)
let arg_get env = 
  let renv =
    ref env
  in
    (
      List.rev
      (
        List.map
          (fun (key, fspc, doc) -> key, (fspc renv), doc)
          env.no_dump.args
      )
    ),
    renv
;;

(** Default no dump environment for dumping env
  *)
let env_no_dump_empty =
  {
    args            = [];
    fn              = "none";
    print_no_export = false;
    print_conf_done = false;
  }
;;

(** Default environment when there is nothing to load
  *)
let env_empty =
  {
    vars            = MapVar.empty;
    temporary_files = SetFn.empty;
    no_dump         = env_no_dump_empty
  }
;;

(** No dump environment to set after load
  *)
let env_no_dump fn =
  {
    args =
      [
        "--override",
        (fun renv -> 
           Arg.Tuple
             (
               let rvr = ref ""
               in
               let rvl = ref ""
               in
                 [
                   Arg.Set_string rvr;
                   Arg.Set_string rvl;
                   Arg.Unit (fun () -> renv := var_add !rvr !rvl !renv)
                 ]
             )
        ),
        "var_val  Override any configuration variable";

        "--print-no-export",
        (fun renv ->
           Arg.Unit 
             (fun () -> 
                renv := 
                  {
                    !renv with 
                        no_dump = 
                          {
                            !renv.no_dump with 
                                print_no_export = true
                          }
                  }
             );
        ),
        " Print even non-printable variable (debug)";
      ];

    fn = fn;
    print_no_export = false;
    print_conf_done = false;
  }
;;

(** Add a temporary file
  *)
let temporary_add fn env = 
  {
    env with 
        temporary_files = SetFn.add fn env.temporary_files
  }
;;

(** Get temporary file list
  *)
let temporary_get env =
  SetFn.elements env.temporary_files
;;

(** Env structure signature, for safe marshalling
  *)
let env_sig =
  Digest.string (Marshal.to_string env_empty [])
;;

(** Save environment on disk.
  *)
let dump env = 
  let chn =
    open_out_bin env.no_dump.fn
  in
    output_value chn env_sig;
    Marshal.to_channel chn 
      {env with no_dump = env_no_dump_empty} 
      [];
    close_out chn
;;

(** Initialize environment.
  *)
let init ?filename pkg_name pkg_version = 
  let fn = 
    Filename.concat 
      (Filename.dirname Sys.argv.(0))
      (
        match filename with 
          | Some f ->
              f
          | None ->
              "buildsys.data"
      )
  in
  let env =
    if Sys.file_exists fn then
      (
        let chn =
          open_in_bin fn
        in
        let env_sig_chn = 
          input_value chn
        in
        let env =
          if env_sig_chn = env_sig then
            Marshal.from_channel chn
          else
            (
              Msg.warn ("Signature of environment has changed since last dump of "^fn);
              Msg.warn "This can be due to change in the 'env' datastructure.";
              Msg.info ("Regenerating "^fn);
              env_empty
            )
        in
          close_in chn;
          env 
      )
    else
      (
        env_empty
      )
  in
  List.fold_left
    (fun env f -> f env)
    env
    [
      var_add "pkg_name" pkg_name;
      var_add "pkg_version" pkg_version;
      (fun env -> {env with no_dump = env_no_dump fn});
    ]
;;

(** Display environment to user.
  *)
let print env =
  let printable_vars =
    MapVar.fold
      (fun name var acc ->
         if var.no_export && not env.no_dump.print_no_export then
           acc
         else
           (name, (var_expand var.value env)) :: acc
      )
      env.vars
      []
  in
  let max_length = 
    List.fold_left
      (fun mx (name, var) -> max (String.length name) mx)
      0
      printable_vars
  in
  print_newline ();
  print_endline "Configuration: ";
  print_newline ();
  List.iter 
    (fun (name,value) ->
       let dots =
         String.make ((max_length - (String.length name)) + 3) '.'
       in
         print_endline (name^": "^dots^" "^value)
    )
    printable_vars;
  print_newline ();
  print_endline "Temporary files: ";
  print_newline ();
  SetFn.iter print_endline env.temporary_files;
  print_newline ();
  {env with no_dump = {env.no_dump with print_conf_done = true}}
;;


let has_changed env1 env2 =
  (MapVar.compare (fun v1 v2 -> String.compare v1.value v2.value) env1.vars env2.vars) != 0
;;

