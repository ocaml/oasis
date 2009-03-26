
(** Act using value collected in environment
    @author Sylvain Le Gall
  *)

module Msg = BaseMessage;;
module Env = BaseEnvironment;;
module Pck = BasePack;;

(** {1 Filename using environment}
  *)
module RelativeFilename =
struct
  module Env = BaseEnvironment

  (** Rebase filename relative to build file
    *)
  let filename fn env =
    Filename.concat (Filename.dirname env.Env.no_dump.Env.fn) fn

  (** Rebase filename and add program suffix, if required
    *)
  let program fn env =
    (filename fn env)^(Env.var_get ~mandatory:true "suffix_program" env)

  (** Rebase filename and add link suffix, if required
    *)
  let link fn env =
    (filename fn env)^(Env.var_get ~mandatory:true "suffix_link" env)

  (* TODO: check for suffix *)
end
;;


(** [replace fn_in env] Replace all string of the form '$(var)' where var is
  * a variable that can be found in [env]. [fn_in] must finish with '.in'.
  * The target file is the source filename without the ending '.in'. The file
  * is updated only if replace all these strings give a different result than
  * the actual target file.
  * 
  * See {!Buffer.substitute} for a complete guide to variable.
  *)
let replace fn_in env =
  (** Make a filename relative to the
    * build file.
    *)
  let () =
    if not (Filename.check_suffix fn_in ".in") then
      failwith ("File "^fn_in^" doesn't end with '.in'")
  in
  let fn_in_rebase =
    RelativeFilename.filename fn_in env 
  in
  let fn_rebase =
    Filename.chop_suffix fn_in_rebase ".in"
  in
  let chn_in =
    open_in fn_in_rebase
  in
  let equal_cur, close_cur, size_cur =
    if Sys.file_exists fn_rebase then
      (
        let chn =
          open_in fn_rebase
        in
          (fun str -> 
             try 
               let line = 
                 input_line chn 
               in 
                 line = str 
             with End_of_file ->
               false
          ),
          (fun () -> close_in chn),
          in_channel_length chn
      )
    else
      (fun _ -> false),
      (fun () -> ()),
      (-1)
  in
  let buffer =
    Buffer.create (2 * (in_channel_length chn_in))
  in
  let rec read_check_replace line_num need_rewrite =
    try
      let line =
        input_line chn_in
      in
      let line_replaced =
        Env.var_expand 
          ~error_extra_message:("at line "^(string_of_int line_num)^
                                " file "^(fn_in_rebase))
          line
          env
      in
      let () = 
        Buffer.add_string buffer line_replaced;
        Buffer.add_char   buffer '\n'
      in
      let line_updated = 
        (not (equal_cur line_replaced))
      in
      let nneed_rewrite =
        need_rewrite || line_updated
      in
        read_check_replace (line_num + 1) nneed_rewrite
    with End_of_file ->
      (
        close_cur ();
        if need_rewrite || (Buffer.length buffer) <> size_cur then
          (
            Msg.info ("Writing file "^fn_rebase);
            let chn_out =
              open_out fn_rebase
            in
              Buffer.output_buffer chn_out buffer
          )
      )
  in
    read_check_replace 1 false;
    Env.temporary_add fn_rebase env
;;

(** Parse command line arguments 
  *)
let parse env =
  let args, renv =
    Env.arg_get env
  in
  let rtargets = 
    ref []
  in
    Arg.parse 
      (Arg.align args)
      (fun str -> rtargets := str :: !rtargets)
      (Env.var_expand 
         "\
         Build system for $pkg_name v$pkg_version\n\
         \n\
         Options: \n\n\
         " 
         env);
    (List.rev !rtargets), !renv
;;

(** Execute a command
  *)
let exec ?(exit_on_error=true) lst env = 
  let cmd = 
    String.concat " " lst 
  in
    Msg.info ("+ "^cmd);
    match Sys.command cmd with
      | 0 ->
          ()
      | i ->
          Msg.warn ("'"^cmd^"' exit with status "^(string_of_int i));
          (if exit_on_error then 
            exit i
          else
            ()
          )
(** Execute every target given on command line 
  *)
let process_targets targets cli_targets (env: Env.env) =
  let process_one_target tgt =
    let actions = 
      List.map snd
       (List.filter (fun (tgt', _) -> tgt' = tgt) targets)
    in
      match actions with 
        | [] -> 
          failwith ("Unknown target "^tgt)
        | _ ->
          List.iter (fun f -> f env) actions
  in
    match cli_targets, targets with 
      | [], (default_target, _) :: _ ->
          (
            Msg.info ("Using default target "^default_target);
            process_one_target default_target
          )
      | lst, _ ->
          List.iter process_one_target lst
;;

(** Build environment using provided series of check to be done
  * and then output corresponding file.
  *)
let main pkg_name pkg_version args checks in_files targets packs =
  let pack = 
    List.fold_left 
      Pck.merge
      {
        Pck.args     = Env.chain args;
        Pck.checks   = Env.chain checks;
        Pck.in_files = in_files;
        Pck.targets  = targets;
      }
      packs
  in
  let env_orig =
    Env.init 
      pkg_name 
      pkg_version
  in
  let env =
    pack.Pck.args env_orig
  in
  let cli_targets, env =
    parse env
  in
  let env = 
    pack.Pck.checks env
  in
  let env =
    List.fold_left 
      (fun env fn_in -> replace fn_in env) 
      env
      pack.Pck.in_files
  in
  let env =
    if Env.has_changed env_orig env then
      (
        Env.dump  env;
        Env.print env
      )
    else
      (
        env
      )
  in
    process_targets 
      pack.Pck.targets 
      cli_targets
      env
;;
