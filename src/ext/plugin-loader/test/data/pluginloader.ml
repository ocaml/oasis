
type action = List | Load of string list

let verbose = ref false

let () =
  let action = ref List in
  let () =
    Arg.parse
      ["-load",
       Arg.String(fun str ->
                    action :=
                    match !action with
                      | List -> Load [str]
                      | Load lst -> Load (lst @ [str])),
       "nm Load a plugin."]
      ignore
      (Sys.executable_name^" [options*]")
  in
  let () =
    PluginLoader.init ["pluginloaderLib"]
  in
  let t =
    {PluginLoader.
     system = "pluginloader";
     msg =
       fun lvl str ->
         let prefix =
           match lvl with
             | `Error -> "E"
             | `Warning -> "W"
             | `Debug -> "D"
         in
           if !verbose then
             prerr_endline (prefix^": "^str)}
  in
    try
      match !action with
        | List ->
            List.iter
              (fun entry ->
                 print_endline
                   (entry.PluginLoader.name^": "^
                    (match entry.PluginLoader.synopsis with
                       | Some str -> str
                       | None -> "<none>")))
              (PluginLoader.list t)
        | Load lst ->
            let _t =
              List.iter (PluginLoader.load t) lst
            in
              print_endline
                ("plugin_loaded: "^
                 (String.concat ", "!PluginloaderLib.registered_plugins))
    with e ->
      prerr_endline (Printexc.to_string e);
      exit 1


