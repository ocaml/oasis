#!/bin/bash

function run {
    NAME=$1
    shift
    echo -n "$NAME ... "
    $@
    CODE=$?
    if [ $CODE -ne 0 ]; then
        echo "... $NAME failed!"
        exit $CODE
    else
        echo "... $NAME OK"
    fi
}

cd "$APPVEYOR_BUILD_FOLDER"

run "OPAM initialization" opam init -y -a
run "Install packages" \
    opam install -y ocamlfind ocaml-data-notation ocamlmod ocamlify
eval $(opam config env)
export OCAML_TOPLEVEL_PATH=$(opam config var toplevel)

run "OASIS Configure step" ocaml setup.ml -configure
run "OASIS Build step" ocaml setup.ml -build

run "Install" ocaml setup.ml -install

echo "------------------------------------------------------------"
echo "Rebuild with dynamic mode"
run "Setup" ./Main.native setup -setup-update dynamic
run "Configure" ocaml setup.ml -info -configure
run "Build" ocaml setup.ml -build
