mkdir dist || true
mkdir tmp || true
cd dist
. "$(dirname $0)/opam.bash" || exit 1
# TODO: use the published version of opam-build-revdeps
opam pin opam-build-revdeps \
  git://github.com/gildor478/opam-build-revdeps.git#opam/unstable
opam-build-revdeps compare --package oasis \
  --version1 latest \
  --version2 latest -pin2 "oasis:$(pwd)/.." \
  --root_dir "$(pwd)/../tmp"

