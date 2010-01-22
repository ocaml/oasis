
(** Run test
    @author Sylvain Le Gall
  *)

open BaseExpr;;
open BaseEnv;;

type t = 
    {
      test_name:               string;
      test_command:            string * string list;
      test_working_directory:  string option;
      test_run:                bool choices;
      test_plugin:             string -> string list -> float;
    }
;;

let test lst extra_args =
  let one_test t =
    if BaseExpr.choose t.test_run then
      begin
        let () = 
          BaseMessage.info 
            (Printf.sprintf "Running test '%s'" t.test_name)
        in
        let cwd = 
          Sys.getcwd ()
        in
        let back_cwd () = 
          BaseMessage.info 
            (Printf.sprintf "Changing directory to '%s'" cwd);
          Sys.chdir cwd
        in
          try 
            let () = 
              match t.test_working_directory with 
                | Some dir -> 
                    BaseMessage.info 
                      (Printf.sprintf "Changing directory to '%s'" dir);
                    Sys.chdir dir
                | None -> ()
            in
            let cmd, args = 
              t.test_command
            in
            let failure_percent =
              t.test_plugin
                (var_expand cmd)
                (List.map 
                   var_expand
                   (args @ (Array.to_list extra_args)))
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
          (Printf.sprintf "Skipping test '%s'" t.test_name);
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
;;

(* END EXPORT *)

open BaseGenCode;;
open OASISTypes;;

let generate lst =

  let generate_one (nm, tst, gen) = 
    REC
      ("BaseTest",
       [
         "test_name",
         STR nm;

         "test_command", 
         (let cmd, args = 
            BaseExec.code_of_command_line 
              tst.test_command
          in
            TPL [cmd; args]);

         "test_working_directory",
         (match tst.test_working_directory with
            | Some wd -> VRT ("Some", [STR wd])
            | None -> VRT ("None", []));

         "test_run", 
         code_of_bool_choices 
           (choices_of_oasis tst.test_run);

         "test_plugin",
         gen.BasePlugin.setup_code;
       ])

  in

    LST (List.map generate_one lst)
;;
