
(** Run test for ocaml-autobuild
    @author Sylvain Le Gall
  *)

open OUnit;;
open TestCommon;;

let _res: test_result list = 
  let ctxt =
    {
      dbug = false;
      has_ocamlopt = 
        (
          try 
            let _s : FilePath.filename = 
              FileUtil.which "ocamlopt"
            in
              true
          with Not_found ->
            false
        );
      ocaml_autobuild =
        FilePath.concat
          (FileUtil.pwd ())
          "../_build/src/OCamlAutobuild.byte";
      ocaml_autobuild_args = [];
    }
  in
    run_test_tt_main
      ("ocaml-autobuild">:::
       [
         TestPropList.tests     ctxt;
         TestOASIS.tests        ctxt;
         TestVersion.tests      ctxt;
         TestFileGenerate.tests ctxt;
         TestBasic.tests        ctxt;
         TestFull.tests         ctxt;
         TestMETA.tests         ctxt;
       ])
;;
