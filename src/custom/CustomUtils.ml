
(** Utilities for Custom plugin.
    @author Sylvain Le Gall
  *)

open BaseEnv;;

let run_and_replace cmd args extra_args =
  BaseExec.run 
    (var_expand cmd)
    (List.map 
       var_expand
       (args @ (Array.to_list extra_args)))
;;

let run_and_replace_test cmd args =
  try
    BaseExec.run cmd args;
    0.0
  with Failure _ ->
    1.0
;;

