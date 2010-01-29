
(** Run test
    @author Sylvain Le Gall
  *)

open BaseEnv
open OASISTypes
open OASISExpr

let test lst pkg extra_args =

  let one_test (test_plugin, test_name, test) =
    if var_choose test.test_run then
      begin
        let () = 
          BaseMessage.info 
            (Printf.sprintf "Running test '%s'" test_name)
        in
        let back_cwd = 
          match test.test_working_directory with 
            | Some dir -> 
                let cwd = 
                  Sys.getcwd ()
                in
                let chdir d =
                  BaseMessage.info 
                    (Printf.sprintf "Changing directory to '%s'" d);
                  Sys.chdir d
                in
                  chdir dir;
                  fun () -> chdir cwd

            | None -> 
                fun () -> ()
        in
          try 
            let failure_percent =
              test_plugin pkg test_name test extra_args 
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
        BaseMessage.info 
          (Printf.sprintf "Skipping test '%s'" test_name);
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
       BaseMessage.warning 
     else
       BaseMessage.info)
      (Printf.sprintf 
         "Tests had a %.2f%% failure rate"
         (100. *. failure_percent))

(* END EXPORT *)
