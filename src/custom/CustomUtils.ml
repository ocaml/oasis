
(** Utilities for Custom plugin.
    @author Sylvain Le Gall
  *)

let run_and_replace cmd args env extra_args =
  let renv =
    ref env
  in
    BaseExec.run 
      (BaseEnvironment.var_expand renv cmd)
      (List.map 
         (BaseEnvironment.var_expand renv)
         (args @ (Array.to_list extra_args)))
;;
