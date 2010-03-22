
(* Identity of OCamlbuild plugin
   @author Sylvain Le Gall
 *)

let name    = "OCamlbuild"

let version = OASISConf.version

let help = OCamlbuildData.readme_template_mkd

let help_extra_vars = []

let help_order = 20
