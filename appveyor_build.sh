#!/bin/bash

cd "$APPVEYOR_BUILD_FOLDER"

echo "Initialize OPAM..."
opam init -y -a
opam install -y ocamlfind ocaml-data-notation ocamlmod ocamlify
eval $(opam config env)
export OCAML_TOPLEVEL_PATH=$(opam config var toplevel)

echo "Compile OASIS..."
ocaml setup.ml -configure
ocaml setup.ml -build

