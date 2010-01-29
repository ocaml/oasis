
(** Generate ocamlbuild build system
    @author Sylvain Le Gall
  *)

open BasePlugin

let plugin_id = "OCamlbuild"

let () =
  plugin_register 
    plugin_id 
    (Build OCamlbuildBuild.plugin_main)
