
(** Run test
    @author Sylvain Le Gall
  *)

open BaseEnv
open OASISMessage
open OASISTypes
open OASISExpr
open OASISGettext

let test lst pkg extra_args =

  let one_test (test_plugin, cs, test) =
    if var_choose test.test_run then
      begin
        let () = 
          info (f_ "Running test '%s'") cs.cs_name
        in
        let back_cwd = 
          match test.test_working_directory with 
            | Some dir -> 
                let cwd = 
                  Sys.getcwd ()
                in
                let chdir d =
                  info (f_ "Changing directory to '%s'") d;
                  Sys.chdir d
                in
                  chdir dir;
                  fun () -> chdir cwd

            | None -> 
                fun () -> ()
        in
          try 
            let failure_percent =
              test_plugin pkg (cs, test) extra_args 
            in
              back_cwd ();
              failure_percent
          with e ->
            begin
              back_cwd ();
              raise e
            end
      end
    else
      begin
        info (f_ "Skipping test '%s'") cs.cs_name;
        0.0
      end
  in
  let res =
    List.map
      one_test
      lst
  in
  let n = 
    float_of_int (List.length res)
  in
  let failure_percent =
    List.fold_left
      (fun r e -> r +. (e /. n))
      0.0
      res
  in
    (if failure_percent > 0.0 then
       warning 
     else
       info)
      (f_ "Tests had a %.2f%% failure rate")
      (100. *. failure_percent)

(* END EXPORT *)
