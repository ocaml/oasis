
open OUnit;;
open TestCommon;;

let tests ctxt =

 "Basic" >:::
 [
   "Help" >::
   (fun () -> 
      assert_command
        ~exit_code:0
        ctxt
        ctxt.ocaml_autobuild
        (ctxt.ocaml_autobuild_args @ ["--help"]))
 ]
;;
   
