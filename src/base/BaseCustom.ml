
(** Run custom command for pre/post hook
    @author Sylvain Le Gall
  *)

open BaseEnv
open OASISTypes
open OASISGettext

(* Expand and run command *)
let run cmd args extra_args =
  BaseExec.run 
    (var_expand cmd)
    (List.map 
       var_expand
       (args @ (Array.to_list extra_args)))

(* Apply a function nested in a custom block
 *)
let hook ?(failsafe=false) cstm f e =
  let optional_command lst = 
    let printer =
      function 
        | Some (cmd, args) -> String.concat " " (cmd :: args)
        | None -> s_ "No command"
    in
      match var_choose ~printer lst with 
        | Some (cmd, args) ->
            begin
              try 
                run cmd args [||]
              with e when failsafe ->
                OASISMessage.warning 
                  (f_ "Command '%s' fail with error: %s")
                  (String.concat " " (cmd :: args))
                  (match e with 
                     | Failure msg -> msg
                     | e -> Printexc.to_string e)
            end
        | None ->
            ()
  in
  let res =
    optional_command cstm.pre_command;
    f e
  in
    optional_command cstm.post_command;
    res
