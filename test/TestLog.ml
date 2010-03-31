
(** BaseLog tests
    @author Sylvain Le Gall
  *)

open OUnit
open TestCommon
open BaseLog

let tests ctxt =
  let test_of_vector (nm, f) =
    nm >:: 
    bracket
      ignore
      f
      (fun () ->
         Sys.remove BaseLog.default_filename)
  in
  let assert_equal_log msg exp =
    assert_equal
      ~msg
      ~printer:(fun lst ->
                  String.concat ", " 
                    (List.map
                       (fun (e, d) -> Printf.sprintf "%S %S" e d)
                       lst))
      exp
      (load ())
  in

    "BaseLog" >:::
    (List.map test_of_vector
       [
         "normal",
         (fun () ->
            register "toto" "mytoto";
            assert_bool 
              "Event toto exists" 
              (exists "toto" "mytoto");
            unregister "toto" "mytoto";
            assert_bool 
              "Event toto doesn't exist" 
              (not (exists "toto" "mytoto")));

         "double",
         (fun () ->
            register "toto" "mytoto";
            assert_equal_log 
              "Log contains 1 element"
              ["toto", "mytoto"];
            register "toto" "mytoto";
            assert_equal_log 
              "Log still contains 1 element"
              ["toto", "mytoto"])
       ])
