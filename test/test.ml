
(** Run test for OASIS
    @author Sylvain Le Gall
  *)

open OUnit;;
open TestCommon;;
open OASISBuiltinPlugins;;

let _res: test_result list = 
  let dbug =
    ref false
  in
  let long =
    ref false
  in
  let only_tests =
    ref []
  in

  let () = 
    Arg.parse
      [
        "-verbose",
        Arg.Set dbug,
        "Run the test in verbose mode.";

        "-only-test",
        Arg.String (fun s -> only_tests := s :: !only_tests),
        "Run only the selected test";

        "-long",
        Arg.Set long,
        "Run long tests";
      ]
      invalid_arg
      ("usage: "^Sys.executable_name^" [options*]")
  in

  let ctxt =
    {
      dbug = !dbug;
      long = !long;
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
      oasis =
        FilePath.make_filename
          [FileUtil.pwd ();FilePath.parent_dir;
           "_build";"src";"OASIS.byte"];
      oasis_args = [];
    }
  in

  let tests = 
    (if !only_tests = [] then
       (fun l -> l)
     else
       (fun l ->
          match test_filter !only_tests l with
            | Some l -> l
            | None -> failwith "No tests to execute"))
      ("OASIS">:::
       [
         TestPropList.tests     ctxt;
         TestOASIS.tests        ctxt;
         TestVersion.tests      ctxt;
         TestFileGenerate.tests ctxt;
         TestBasic.tests        ctxt;
         TestFull.tests         ctxt;
         TestMETA.tests         ctxt;
         TestLog.tests          ctxt;
       ])
  in

    run_test_tt ~verbose:!dbug tests
;;
