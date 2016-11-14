mkdir dist || true
mkdir tmp || true
. "$(dirname $0)/opam.bash" || exit 1
# TODO: use the published version of opam-build-revdeps
opam pin add opam-build-revdeps \
  git://github.com/gildor478/opam-build-revdeps.git#opam/unstable
# TODO: un-exclude when upgrading to Debian Stretch
# - maxminddb
# - brotli
# - zstd
opam-build-revdeps compare --package oasis \
  --only zipperposition \
  --only zmq \
  --only zstd \
  --exclude qfs \
  --exclude maxminddb \
  --exclude brotli \
  --exclude zstd \
  --version1 latest \
  --version2 latest --pin2 "oasis:$(pwd)/sut" \
  --html_output "dist/index.html"

