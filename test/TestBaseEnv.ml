
(** Tests for BaseEnv.
    @author Sylvain Le Gall
*)

open OUnit2
open OASISContext

let tests =
  "BaseEnv" >:::
  [
    "default_filename synced" >::
    (fun test_ctxt ->
       let ctxt = TestCommon.oasis_ctxt test_ctxt in
       assert_equal
         ~printer:(Printf.sprintf "%S")
         (ctxt.srcfs#string_of_filename BaseEnv.default_filename)
         BaseEnvLight.default_filename);
  ]
