
(** Configure using OCaml-autobuild
    @author Sylvain Le Gall
  *)

module Msg = BaseMessage;;
module Env = BaseEnvironment;;

(* TODO: use it
(** Evaluate expression *)
let rec expr_eval ctxt =
  function 
    | ETrue  ->
        true
    | EFalse -> 
        false
    | ENot e -> 
        expr_eval ctxt e 
    | EAnd (e1, e2) ->
        (expr_eval ctxt e1) && (expr_eval ctxt e2)
    | EOr (e1, e2) -> 
        (expr_eval ctxt e1) || (expr_eval ctxt e2)
    | EFlag nm ->
        (
          (* TODO *)
          false
        )
    | ETest (nm, vl) ->
        (
          (* TODO *)
          false
        )
;;
 *)

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
  let fn_rebase =
    Filename.chop_suffix fn_in ".in"
  in
  let chn_in =
    open_in fn_in
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
                                " file "^fn_in)
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
    env
;;

(** Build environment using provided series of check to be done
  * and then output corresponding file.
  *)
let configure pkg_name pkg_version packs argv =
  let {BasePack.args     = args;
       BasePack.checks   = checks; 
       BasePack.in_files = in_files} = 
      BasePack.merge (BasePack.default :: packs)
  in

  (* Data file for setup.ml *)
  let fn =
    (Filename.chop_extension Sys.argv.(0))^".data"
  in

  (* Build initial environment *)
  let env_org =
    Env.init 
      fn
      pkg_name 
      pkg_version
  in
  (* Parse command line *)
  let env =
    BaseArgExt.parse argv args env_org
  in

  (* Do some check *)
  let env =
    checks env
  in

  (* Replace data in file *)
  let env =
    List.fold_left 
      (fun env fn_in -> replace fn_in env) 
      env
      in_files
  in
    if not (Env.equal env_org env) then
      (
        Env.dump  fn env;
        Env.print env
      )
;;
