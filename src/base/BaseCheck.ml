
(** {1 Checking for particular features} 
  *)

module Env = BaseEnvironment;;
module Ver = BaseVersion;;
module Msg = BaseMessage;;

(** Look for a program among a list of alternative program
  * the first found is returned. 
  *)
let prog_best prg prg_lst =
  Env.var_cache prg
    (fun env ->
       let alternate = 
         List.fold_left 
           (fun res e ->
              match res with 
                | Some _ -> res
                | None ->
                    try
                      Some (BaseFileUtil.which e)
                    with Not_found ->
                      None)
           None
           prg_lst
       in
         match alternate with
           | Some prg -> prg, env
           | None -> raise Not_found
    )
;;

(** Check the presence of a particular program.
  *)
let prog prg =
  prog_best prg [prg]
;;

(** Check the presenc of a program or its native version
  *)
let prog_opt prg = 
  prog_best prg [prg^".opt"; prg]
;;

(** Check version, following Sys.ocaml_version convention
  *)
let version feature var_prefix str_comparator fversion = 
  (* Really compare version provided *)
  let comparator =
    (* TODO: remove *)
    BaseVersionTools.comparator_of_string str_comparator
  in
  let var = 
    var_prefix^"_version_"^(BaseVersionTools.varname_of_comparator comparator)
  in
    Env.var_cache ~hide:true var
      (fun env ->
         let () = 
           Msg.checking (feature^" version "^str_comparator);
         in
         let version, env =
           match fversion env with 
             | "[Distributed with OCaml]", env ->
                 (* TODO: this is not sure ! *)
                 Sys.ocaml_version, env
             | res ->
                 res
         in
           if Ver.comparator_apply version comparator then
             version, env
           else
             raise Not_found
      )
;;

(** Check for findlib package
  *)
let package ?version_comparator pkg =
  let findlib_dir pkg = 
    let dir = 
      BaseExec.run_read_one_line
        "ocamlfind"
        ["query"; "-format"; "%d"; pkg]
    in
      if Sys.is_directory dir then
        dir
      else
        failwith
          (Printf.sprintf
             "When looking for findlib package %s, \
              directory %s return doesn't exist"
             pkg dir)
  in
  let findlib_version pkg =
    BaseExec.run_read_one_line 
      "ocamlfind"
      ["query"; "-format"; "%v"; pkg]
  in
  Env.var_cache ("pkg_"^pkg)
    (fun env ->
       let default_msg =
         "findlib package "^pkg
       in
       let () = 
         Msg.checking default_msg
       in
       let dir =
         findlib_dir pkg
       in
         match version_comparator with 
           | Some str_cmp ->
               (
                 let _, env = 
                   version 
                     default_msg 
                     ("pkg_"^pkg)
                     str_cmp 
                     (fun env -> 
                        findlib_version pkg, env)
                     env
                 in
                   dir, env
               )
           | None -> 
               dir, env
    )
;;

(** Run checks *)
let run checks env =
  List.fold_left
    (fun env chk -> snd (chk env))
    env
    checks
;;
