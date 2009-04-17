
(** {1 Checking for particular features} 
  *)

open FileUtil;;
open FileUtil.StrUtil;;
open FilePath.DefaultPath;;

module Env = BaseEnvironment;;
module Ver = BaseVersion;;

let () = 
  Findlib.init ()
;;

(** Look for a program among a list of alternative program
  * the first found is returned. 
  *)
let prog_best prg prg_lst =
  Env.var_cache prg
    (fun env ->
       try 
         let alternate = 
           (* TODO: don't ignore result *)
           List.find 
             (fun prg -> try ignore(which prg); true with Not_found -> false)
             prg_lst
         in
           (which alternate), env
       with Not_found ->
         (
           raise Not_found
         )
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
         (* TODO: provide description to variable definition 
         let () = 
           Msg.checking (feature^" version "^str_comparator);
         in
          *)
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
  Env.var_cache ("pkg_"^pkg)
    (fun env ->
       try 
         let default_msg =
           "findlib package "^pkg
         in
         (* TODO: provide description to variable definition + error
          * reporting
         let () = 
           Msg.checking default_msg
         in
          *) 
         let dir =
           Findlib.package_directory pkg
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
                          Findlib.package_property [] pkg "version", env)
                       env
                   in
                     dir, env
                 )
             | None -> 
                 dir, env
       with Fl_package_base.No_such_package _ ->
         raise Not_found
    )
;;

(** Run checks *)
let run checks env =
  List.fold_left
    (fun env chk -> snd (chk env))
    env
    checks
;;
