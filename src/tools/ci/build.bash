. "$(dirname $0)/packages.bash" || exit 1
. "$(dirname $0)/opam.bash" || exit 1
mkdir dist || true
opam install $OPAM_PKGS
opam install omake.0.9.8.6-0.rc1
export OCAMLRUNPARAM=b
ocaml setup.ml -distclean
ocaml setup.ml -configure --enable-tests --enable-devel ${CONFIGURE_ARGS}
ocaml setup.ml -build
ocaml setup.ml -test ${TEST_ARGS}
