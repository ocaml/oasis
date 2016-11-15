mkdir dist || true
mkdir tmp || true
. "$(dirname $0)/opam.bash" || exit 1

if [ "x${OPAM_BUILD_REVDEPS_UNSTABLE}" == "xyes" ] ; then
  opam pin add opam-build-revdeps \
    'git://github.com/gildor478/opam-build-revdeps.git#opam/unstable'
else
  opam install opam-build-revdeps
fi

# TODO: un-exclude when upgrading to Debian Stretch
# - maxminddb
# - brotli
# - zstd
opam-build-revdeps compare --package oasis ${OPAM_BUILD_REVDEPS_ARGS} \
  --exclude qfs \
  --exclude maxminddb \
  --exclude brotli \
  --exclude zstd \
  --ocaml_version 4.03.0 \
  --version1 latest \
  --version2 latest --pin2 "oasis:$(pwd)/sut" \
  --html_output "dist/index.html" \
  --css_output "dist/index.css"

