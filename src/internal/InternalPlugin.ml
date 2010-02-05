
(** Internal configure and install scheme
    @author Sylvain Le Gall
  *)

open BasePlugin

let plugin_id = "Internal"

let () = 
  plugin_register 
    plugin_id 
    (Configure 
       InternalConfigure.plugin_main);
  plugin_register 
    plugin_id 
    (Install 
       (InternalInstall.plugin_install_main, 
        InternalInstall.plugin_uninstall_main))

