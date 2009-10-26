
(** Runtime support for autobuild/OCamlbuild
    @author Sylvain Le Gall
  *)

open BaseStandardVar;;

type target =
  | Std of string
  | Rename of string * string
;;

type t = 
  (target BaseExpr.choices) list
;;

let cond_targets_hook =
  ref (fun lst -> lst)
;;

let build cond_targets env argv =
  (* Fix special arguments depending on environment *)
  let env_args =
    List.flatten
      [
        if (os_type env) = "Win32" then
          [
            "-classic-display"; 
            "-no-log"; 
            "-install-lib-dir"; 
            (Filename.concat (standard_library env) "ocamlbuild")
          ] 
        else
          [];
    
        if (ocamlbest env) = "byte" || (os_type env) = "Win32" then
          [
            "-byte-plugin" 
          ]
        else
          [];
      ]
  in

  let ocamlbuild_run rtargets = 
    let args = 
      List.rev_append rtargets (Array.to_list argv)
    in
      BaseExec.run (ocamlbuild env) (env_args @ args)
  in

  let in_build_dir fn =
    Filename.concat "_build" fn
  in

  let last_rtargets =
    List.fold_left
      (fun acc (choices, tgt) ->
         if BaseExpr.choose choices env then 
           match tgt with 
             | Std nm -> 
                 nm :: acc
             | Rename (src, tgt) ->
                 ocamlbuild_run (src :: acc);
                 BaseFileUtil.cp 
                   (in_build_dir src) 
                   (in_build_dir tgt);
                 []
         else
           acc)
      []
      (!cond_targets_hook cond_targets)
  in
    ocamlbuild_run last_rtargets
;;

let clean () = 
  (* TODO use ocamlbuild *)
  BaseExec.run "ocamlbuild" ["-clean"]
;;

