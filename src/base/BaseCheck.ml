
(** {1 Checking for particular features} 
  *)

open BaseEnvironment;;
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
      feature 
      var_prefix 
      (str_cmp, cmp, var_cmp) 
      fversion = 
  (* Really compare version provided *)
  let var = 
    var_prefix^"_version_"^var_cmp
  in
    var_set 
      ~hide:true 
      var
      (lazy (* TODO: re-enable
          let () = 
            Msg.checking (feature^" version "^str_cmp);
          in
           *)
         (let version =
            match fversion () with 
              | "[Distributed with OCaml]" ->
                  (* TODO: this is not sure ! *)
                  Sys.ocaml_version
              | res ->
                  res
          in
            if Ver.comparator_apply version cmp then
              version
            else
              raise Not_found))
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
    (* TODO re-enable
      let default_msg =
        "findlib package "^pkg
      in
      let () = 
        Msg.checking default_msg
      in
     *)
  let vl =
    var_define
      ("pkg_"^pkg)
      (lazy (findlib_dir pkg))
      env
  in
    (
      match version_comparator with 
        | Some ver_cmp ->
            version 
              (*default_msg *) ""
              ("pkg_"^pkg)
              ver_cmp
              (fun () -> findlib_version pkg)
              env
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
