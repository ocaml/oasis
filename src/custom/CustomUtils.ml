
(** Utilities for Custom plugin.
    @author Sylvain Le Gall
  *)

let run_and_replace cmd args env extra_args =
  BaseExec.run 
    (BaseEnvRW.var_expand env cmd)
    (List.map 
       (BaseEnvRW.var_expand env)
       (args @ (Array.to_list extra_args)))
;;
