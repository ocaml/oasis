(* OASIS_START *)
(* OASIS_STOP *)
open Ocamlbuild_plugin
let dispatch_add_cc e =
    match e with
      | After_rules ->
          flag ["compile"; "c"] (S[A "-cc"; A "g++"])
      | _ -> ()
;;

Ocamlbuild_plugin.dispatch
  (MyOCamlbuildBase.dispatch_combine
    [dispatch_default; dispatch_add_cc]);;
