
(** Utilities for Custom plugin.
    @author Sylvain Le Gall
  *)

let run_and_replace cmd args env extra_args =
  BaseExec.run 
    (BaseEnvironment.var_expand env cmd)
    (List.map 
       (BaseEnvironment.var_expand env)
       (args @ (Array.to_list extra_args)))
;;
