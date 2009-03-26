
(** Run test for ocaml-autobuild
    @author Sylvain Le Gall
  *)

open OUnit;;
open TestCommon;;

let _res: test_result list = 
  let ctxt =
    {
      dbug = false;
    }
  in
    run_test_tt_main
      ("ocaml-autobuild">:::
       [
         TestOASIS.tests   ctxt;
         TestVersion.tests ctxt;
       ])
;;
