
(** {1 Checking for particular features} 
  *)

open FileUtil;;
open FileUtil.StrUtil;;
open FilePath.DefaultPath;;

module Msg = BaseMessage;;
module Env = BaseEnvironment;;
module Ver = BaseVersion;;

let () = 
  Findlib.init ()
;;

(** Get only env from check result 
  *)
let fenv fchk env =
  snd (fchk env)
;;

(** Look for a program among a list of alternative program
  * the first found is returned. 
  *)
let prog_best prg prg_lst =
  Env.cache prg
    (fun env ->
       let () = 
         Msg.checking prg
       in
         try 
           let alternate = 
             List.find 
               (fun prg -> try ignore(which prg); true with Not_found -> false)
               prg_lst
           in
             Msg.result_wrap (which alternate), env
         with Not_found ->
           (
             Msg.result "Not found";
             raise Not_found
           )
    )
;;

(** Check the presence of a particular program.
  *)
let prog ?if_not_found prg =
  prog_best 
    prg 
    [prg]
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
    Env.cache ~hide:true var
      (fun env ->
         let () = 
           Msg.checking (feature^" version "^str_comparator);
         in
         let version =
           match fversion () with 
             | "[Distributed with OCaml]" ->
                 Sys.ocaml_version
             | version ->
                 version
         in
           if Ver.comparator_apply version comparator then
             Msg.result_wrap version, env
           else
             (
               Msg.result (" doesn't match ("^version^" "^str_comparator^")");
               raise Not_found
             )
      )
;;

(** Check for findlib package
  *)
let package ?version_comparator pkg =
  let default_msg =
    "findlib package "^pkg
  in
  let fpkg =
    Env.cache ("pkg_"^pkg)
      (fun env ->
         try 
           let () = 
             Msg.checking default_msg
           in
           let dir =
             Findlib.package_directory pkg
           in
           let () = 
             Msg.result dir
           in
             dir, env
         with
           | Fl_package_base.No_such_package _ ->
               raise Not_found
      )
  in
    match version_comparator with 
      | Some str_cmp ->
          (fun env ->
             let (dir, env) =
               fpkg env
             in
             let (_, env) = 
               version 
                 default_msg 
                 ("pkg_"^pkg)
                 str_cmp 
                 (fun () -> Findlib.package_property [] pkg "version") 
                 env
             in
               dir, env
          )
      | None -> 
          fpkg
;;

