ci = require("ci")
oasis = require("oasis")
git = require("git")

ci.init()
-- godi.init()
ci.prependenv("PATH", "/usr/opt/godi/bin")
ci.prependenv("PATH", "/usr/opt/godi/sbin")

git.init()
oasis.init()

oasis.std_process("--enable-tests")

-- Create documentation package.
ci.exec("make", "doc-dist")
