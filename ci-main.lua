bootstrap = require("bootstrap")

bootstrap.init()

ci = require("ci")
godi = require("godi")
oasis = require("oasis")
darcs = require("darcs")

ci.init()
godi.init()
darcs.init()
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

ci.exec("make", "fixperms")
oasis.std_process("--enable-tests")
darcs.create_tag(oasis.package_version())
