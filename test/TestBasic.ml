
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
        ctxt.oasis
        (ctxt.oasis_args @ ["--help"]))
 ]
;;
   
