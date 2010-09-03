

(** Tests the subcommand query of OASIS
    @author Sylvain Le Gall
  *)

open TestCommon
open OUnit

let tests ctxt =
  let test_of_vector (fn, qa) = 
    let q, a = 
      List.split qa
    in
    let test_name = 
      Printf.sprintf 
        "query('%s', %s)" 
        fn (String.concat ", " q)
    in
      test_name >::
      (fun () ->
         assert_oasis_cli 
           ~output:((String.concat "\n" a)^"\n")
           ctxt
           (["query"; "-oasis"; in_data fn] @ q))
  in

    "query" >:::
    (List.map test_of_vector
       [
         "test1.oasis",
         ["version", "0.0.1";
          "name", "oasis"];

         "test1.oasis",
         ["Flag(devmod).Default", "false"];

         "test10.oasis",
         ["ListSections", 
          "Test(main)\nFlag(test)"];

         "test10.oasis",
         ["ListFields",
          "OASISFormat\
           \nName\
           \nVersion\
           \nSynopsis\
           \nLicenseFile\
           \nAuthors\
           \nCopyrights\
           \nMaintainers\
           \nLicense\
           \nConfType\
           \nBuildType\
           \nInstallType\
           \nCategories\
           \nFilesAB\
           \nPlugins\
           \nBuildDepends\
           \nBuildTools\
           \nXDevFilesMakefileNoTargets\
           \nXDevFilesEnableMakefile\
           \nXDevFilesEnableConfigure\
           \nXStdFilesREADMEFilename\
           \nXStdFilesREADME\
           \nXStdFilesINSTALLFilename\
           \nXStdFilesINSTALL\
           \nXStdFilesAUTHORSFilename\
           \nXStdFilesAUTHORS\
           \nTest(main).Type\
           \nTest(main).TestTools\
           \nTest(main).Run\
           \nFlag(test).Description"]
       ])
