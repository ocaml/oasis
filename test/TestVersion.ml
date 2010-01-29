
(** Tests for BaseVersion
    @author Sylvain Le Gall
  *)

open OUnit
open TestCommon
open OASISVersion

let tests ctxt =

  let version_compare_of_vector (v1, v2, exp) =
    (Printf.sprintf "version_compare %S %S" v1 v2) >::
    (fun () ->
       let norm_sign i =
         if i = 0 then
           0
         else if i < 0 then
           -1
         else
           1
       in
         assert_equal
           ~msg:(Printf.sprintf 
                   "Result of '%s' and '%s' comparison" 
                   v1 
                   v2)
           ~printer:string_of_int
            exp
            (norm_sign 
               (version_compare 
                  (version_of_string v1)
                  (version_of_string v2))))
  in

  let comparator_apply_of_vector (v, c, exp) =
    (Printf.sprintf "comparator_apply %S %S" v c) >::
    (fun () ->
       let op =
         comparator_of_string c
       in
         assert_equal
           ~msg:(Printf.sprintf
                   "Result of applying comparator '%s' to '%s'"
                   c
                   v)
           ~printer:string_of_bool
           exp
           (comparator_apply 
              (version_of_string v)
              op))
  in

  "Version" >:::
  [
    "compare" >:::
    (List.map version_compare_of_vector
       [
         "1.0.2", "1.0.2", 0;
         "1.0.1", "1.0.2", -1;
         "1.0.3", "1.0.2", 1;
         "0.6.0", "0.7",   -1;
       ]);

    "comparator" >:::
    (List.map comparator_apply_of_vector
       [
         "1.0.2", ">= 1.0.2", true;
         "1.0.2", "= 1.0.2", true;
         "1.0.2", "> 1.0.2", false;
         "1.0.1", ">= 1.0.2", false;
       ]);
  ]
