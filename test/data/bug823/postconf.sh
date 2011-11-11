#!/bin/sh

set -e 

. ./setup.data

set -x 

echo "testing ocamlopt"
test "$ocamlopt" = "myocamlopt"

echo "testing myvar"
test "$myvar" = "true"
