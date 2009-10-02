
(** Runtime support for autobuild/OCamlbuild
    @author Sylvain Le Gall
  *)

open BaseStandardVar;;

let cond_targets_hook =
  ref (fun lst -> lst)
;;

let build cond_targets env argv =
  let rtargets =
    List.fold_left
      (fun acc (choices, tgt) ->
         if BaseExpr.choose choices env then 
           tgt :: acc
         else
           acc)
      []
      (!cond_targets_hook cond_targets)
  in
  let args = 
    List.rev_append rtargets (Array.to_list argv)
  in
  let args =
    if (os_type env) = "Win32" then
      [
        "-classic-display"; 
        "-no-log"; 
        "-install-lib-dir"; 
        (Filename.concat (standard_library env) "ocamlbuild")
      ] @ args
    else
      args
  in
  let args =
    if (ocamlbest env) = "byte" || (os_type env) = "Win32" then
      "-byte-plugin" :: args
    else
      args
  in
    BaseExec.run (ocamlbuild env) args
;;

let clean () = 
  (* TODO use ocamlbuild *)
  BaseExec.run "ocamlbuild" ["-clean"]
;;

