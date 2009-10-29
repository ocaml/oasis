
(** {1 Checking for particular features} 
  *)

open BaseEnvRW;;
module Ver = BaseVersion;;
module Msg = BaseMessage;;

(** Look for a program among a list of alternative program
  * the first found is returned. 
  *)
let prog_best prg prg_lst =
  var_define
    prg 
    (lazy 
       (let alternate = 
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
            | Some prg -> prg
            | None -> raise Not_found))
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

let ocamlfind = prog "ocamlfind";;

(** Check version, following Sys.ocaml_version convention
  *)
let version 
      var_prefix 
      (str_cmp, cmp, var_cmp) 
      fversion 
      env = 
  (* Really compare version provided *)
  let var = 
    var_prefix^"_version_"^var_cmp
  in
    var_define 
      ~hide:true 
      var
      (lazy
         (let version =
            match fversion env with 
              | "[Distributed with OCaml]" ->
                  (var_get "ocaml_version" env)
              | res ->
                  res
          in
            prerr_endline version;
            if Ver.comparator_apply version cmp then
              version
            else
              failwith 
                (Printf.sprintf
                   "Cannot satisfy version constraint on %s: %s (version: %s)"
                   var_prefix
                   str_cmp
                   version)))
      env
;;

(** Check for findlib package
  *)
let package ?version_comparator pkg env =
  let findlib_dir pkg = 
    let dir = 
      BaseExec.run_read_one_line
        (ocamlfind env)
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
      (ocamlfind env)
      ["query"; "-format"; "%v"; pkg]
  in
  let vl =
    var_define
      ("pkg_"^pkg)
      (lazy (findlib_dir pkg))
      env
  in
    (
      match version_comparator with 
        | Some ver_cmp ->
            var_ignore
              (version 
                 ("pkg_"^pkg)
                 ver_cmp
                 (fun _ -> findlib_version pkg)
                 env)
        | None -> 
            ()
    );
    vl
;;

(** Run checks *)
let run checks env =
  List.iter
    (fun chk -> var_ignore (chk env))
    checks
;;
