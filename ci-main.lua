bootstrap = require("bootstrap")

bootstrap.init()

ci = require("ci")
godi = require("godi")
oasis = require("oasis")
git = require("git")

ci.init()
godi.init()
git.init()
oasis.init()

godi.bootstrap("3.12")
godi.update()
godi.upgrade()
godi.build_many(
  {"godi-findlib",
   "godi-ocaml-fileutils",
   "godi-ocaml-data-notation",
   "godi-ocaml-expect",
   "godi-ounit",
   "apps-ocamlmod",
   "apps-ocamlify"})

oasis.std_process("--enable-tests")
git.create_tag(oasis.package_version())
