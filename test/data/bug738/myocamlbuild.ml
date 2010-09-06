(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let () =
  dispatch
    (fun hook ->
       dispatch_default hook;
       match hook with
         | After_rules ->
             rule ".bar --> .ml" ~dep:"%.bar" ~prod:"%.ml"
               (fun env _ -> cp (env "%.bar") (env "%.ml"))

         | _ ->
             ())
