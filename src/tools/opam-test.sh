#!/bin/bash

set -e
set -x

UUID=$(uuidgen)
HTMLDIR="/tmp/${UUID}"
INCREMENTAL=true

build_and_test () {
  local _version="$(ocamlc -version)"
  local _backup="_build-${_version}"
  ocaml setup.ml -distclean
  if $INCREMENTAL && [ -e "${_backup}" ] ; then
    mv "${_backup}" _build
  fi
  ocaml setup.ml -configure --enable-tests
  ocaml setup.ml -build
  ocaml setup.ml -test "$@" || true
  if $INCREMENTAL ; then
    mv _build "${_backup}"
  fi
  ocaml setup.ml -distclean
}

mkdir "${HTMLDIR}"

for switch in $(opam switch list -s -i); do
  opam switch "${switch}"
  eval $(opam config env)
  build_and_test "$@"
done

# echo "Visit file://${HTMLDIR}"
# google-chrome "file://${HTMLDIR}"
