
OPAM_PKGS="base-bytes base-unix ocamlfind ocaml-data-notation pcre expect fileutils ounit ocamlify ocamlmod omake"

export OPAMYES=1
if [ -f "$HOME/.opam/config" ]; then
    opam update
    opam upgrade
else
    opam init
fi
if [ -n "${OPAM_SWITCH}" ]; then
    opam switch ${OPAM_SWITCH}
fi
eval `opam config env`

opam install $OPAM_PKGS

export OCAMLRUNPARAM=b

ocaml setup.ml -configure --enable-tests
ocaml setup.ml -build
ocaml setup.ml -test
