#!/bin/sh
ocamlbuild autobuild.cma && ocamlfind install autobuild META _build/autobuild.{cma,cmi}
