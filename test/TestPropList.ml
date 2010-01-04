
(** Tests for PropList
    @author Sylvain Le Gall
  *)

open OUnit;;
open TestCommon;;
open PropList;;
open PropList.Field;;

let tests ctxt =
  "PropList" >::
  (fun () ->
     let toto =
       Field.create
         ~default:1
         ~parse:(no_context int_of_string)
         ()
     in

     let data =
       Data.create ()
     in

     let assert_equal_int =
       assert_equal 
         ~printer:string_of_int
     in

       (* Default *)
       assert_equal_int
         1
         (fget data toto);

       (* Assign *)
       fset data toto 2;
       assert_equal_int
         2
         (fget data toto);

       (* Parse *)
       fsets data toto "3";
       assert_equal_int
         3
         (fget data toto);

       ())
;;
