
(** Run test for ocaml-autobuild
    @author Sylvain Le Gall
  *)

open OUnit;;

let _res: test_result list = 
  let ctxt =
    ()
  in
    run_test_tt_main
      ("ocaml-autobuild">:::
       [
         TestOASIS.tests ctxt;
       ])
;;
