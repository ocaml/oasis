
(** Runtime support for autobuild/OCamlbuild
    @author Sylvain Le Gall
  *)

let build cond_targets argv =
  let env =
    BaseEnvironment.load "setup.data"
  in
  let rtargets, env =
    List.fold_left
      (fun (acc, env) (choices, tgt) ->
         let choice, env =
           BaseExpr.choose choices env
         in
           (if choice then tgt :: acc else acc), env)
      ([], env)
      cond_targets
  in
    BaseExec.run 
      "ocamlbuild" 
      (List.rev_append rtargets (Array.to_list argv))
;;

let clean () = 
  BaseExec.run "ocamlbuild" ["-clean"]
;;

