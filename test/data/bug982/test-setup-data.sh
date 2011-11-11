#!/bin/sh

set -e

source "./setup.data"

set -x

test "x$ocamlc" != "x"

make -f Makefile.setup-data
