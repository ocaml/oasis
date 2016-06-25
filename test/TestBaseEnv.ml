
(** Tests for BaseEnv.
    @author Sylvain Le Gall
*)

open OUnit2

let tests =
  "BaseEnv" >:::
  [
    "default_filename synced" >::
    (fun _ ->
       assert_equal
         ~printer:(Printf.sprintf "%S")
         (Lazy.force (BaseEnv.default_filename))
         BaseEnvLight.default_filename);
  ]
