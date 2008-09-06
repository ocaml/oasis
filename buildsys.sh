#!/bin/sh
#+ AUTOBUILD_START 
#+ DO NOT EDIT THIS PART 
#+ tag content_digest "dcfbb0a7bb1296b0c59846f3c8afb41c" 
#+ tag header_digest "320cce8ccaf83b8487d6a544489d0edc" 
PACKAGES="$PACKAGES findlib fileutils"

OCAMLFLAGS=$(ocamlfind query -format "-I %d %a" -separator " " -suffix "" -r -predicates byte,toploop $PACKAGES)

exec ocaml $OCAMLFLAGS $(dirname $0)/buildsys.ml $*
#+ AUTOBUILD_STOP 
