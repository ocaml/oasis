
open OUnit2
open OASISString


let tests =
  "OASISString" >:::
  [
    "split_newline" >::
    (fun test_ctxt ->
       let assert_string_list_equal =
         assert_equal
           ~printer:(fun lst ->
                       String.concat ", "
                         (List.map (Printf.sprintf "%S") lst))
       in
       assert_string_list_equal
         []
         (split_newline "");
       assert_string_list_equal
         [""; ""]
         (split_newline "\n");
       assert_string_list_equal
         ["abcd"; ""]
         (split_newline "abcd\n");
       ());
  ]
