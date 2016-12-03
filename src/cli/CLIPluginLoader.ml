
let () =
  PluginLoader.init CLIPluginsLoaded.exec_oasis_build_depends_rec

(** Plugin for the command line. *)
let plugin_cli_t () =
  {
    PluginLoader.
    system = "oasis-cli";
    msg =
      (fun lvl str ->
         OASISMessage.generic_message ~ctxt:!BaseContext.default lvl "%s" str)
  }

(** Plugin for OASIS. *)
let plugin_t () =
  {
    PluginLoader.
    system = "oasis";
    msg =
      (fun lvl str ->
         OASISMessage.generic_message ~ctxt:!BaseContext.default lvl "%s" str)
  }

let load_oasis_plugin nm =
  try
    PluginLoader.load (plugin_t ()) nm;
    true
  with PluginLoader.Plugin_not_found _ ->
    false
