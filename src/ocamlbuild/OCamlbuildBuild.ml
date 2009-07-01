
(** Runtime support for autobuild/OCamlbuild
    @author Sylvain Le Gall
  *)

module Env = BaseEnvironment 
;;

let cond_targets_hook =
  ref (fun lst -> lst)
;;

let build cond_targets env argv =
  let rtargets, env =
    List.fold_left
      (fun (acc, env) (choices, tgt) ->
         let choice, env =
           BaseExpr.choose choices env
         in
           (if choice then tgt :: acc else acc), env)
      ([], env)
      (!cond_targets_hook cond_targets)
  in
  let eget =
    Env.get env
  in
  let ocamlbuild = 
    eget "ocamlbuild"
  in
  let args = 
    List.rev_append rtargets (Array.to_list argv)
  in
  let args =
    if (eget "os_type") = "Win32" then
      [
        "-classic-display"; 
        "-no-log"; 
        "-install-lib-dir"; 
        (Filename.concat (eget "ocaml_stdlib") "ocamlbuild")
      ] @ args
    else
      args
  in
  let args =
    if (eget "ocamlbest") = "byte" || (eget "os_type") = "Win32" then
      "-byte-plugin" :: args
    else
      args
  in
    BaseExec.run ocamlbuild args
;;

let clean () = 
  BaseExec.run "ocamlbuild" ["-clean"]
;;

