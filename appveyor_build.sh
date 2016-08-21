#!/bin/bash
################################################################################
#  OASIS: architecture for building OCaml libraries and applications           #
#                                                                              #
#  Copyright (C) 2011-2016, Sylvain Le Gall                                    #
#  Copyright (C) 2008-2011, OCamlCore SARL                                     #
#                                                                              #
#  This library is free software; you can redistribute it and/or modify it     #
#  under the terms of the GNU Lesser General Public License as published by    #
#  the Free Software Foundation; either version 2.1 of the License, or (at     #
#  your option) any later version, with the OCaml static compilation           #
#  exception.                                                                  #
#                                                                              #
#  This library is distributed in the hope that it will be useful, but         #
#  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  #
#  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          #
#  details.                                                                    #
#                                                                              #
#  You should have received a copy of the GNU Lesser General Public License    #
#  along with this library; if not, write to the Free Software Foundation,     #
#  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               #
################################################################################

set -e

function run {
    NAME=$1
    shift
    echo "$NAME ... "
    $@
    CODE=$?
    if [ $CODE -ne 0 ]; then
        echo "... $NAME failed!"
        return $CODE
    else
        echo "... $NAME OK"
    fi
}

cd "$APPVEYOR_BUILD_FOLDER"

run "OPAM Initialization" opam init --use-internal-solver -y -a
run "OPAM Install packages" opam install --use-internal-solver -y \
  ocamlfind ocamlmod ocamlify fileutils ounit
eval $(opam config env)
export OCAML_TOPLEVEL_PATH=$(opam config var toplevel)

run "OASIS Uninstall previously installed version" ocaml setup.ml -uninstall || true
run "OASIS Distclean" ocaml setup.ml -distclean || true
for i in oasis plugin-loader ; do
  run "ocamlfind remove $i" ocamlfind remove "$i" || true
done
run "OASIS Configure" ocaml setup.ml -configure \
  --override ocamlbuildflags -classic-display
run "OASIS Build" ocaml setup.ml -build
run "OASIS Install" ocaml setup.ml -install

echo "------------------------------------------------------------"
echo "Rebuild with dynamic mode"
run "Setup" ./Main.native setup -setup-update dynamic
run "Configure" ocaml setup.ml -configure
run "Build" ocaml setup.ml -build
