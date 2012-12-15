#!/bin/sh -ex

OASIS=../../../_build/src/cli/Main.byte
OCAMLDIR=`ocamlc -where`

${OASIS} setup
ocaml setup.ml -configure
ocaml setup.ml -build -classic-display
cd _build
gcc -c -I${OCAMLDIR} ../main.c
gcc -L${OCAMLDIR} main.o B.nobj.o -lasmrun
./a.out
cd ..
rm -f setup.data setup.log setup.ml myocamlbuild.ml _tags
rm -rf _build
