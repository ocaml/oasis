#!/bin/bash
################################################################################
#  OASIS: architecture for building OCaml libraries and applications           #
#                                                                              #
#  Copyright (C) 2011-2013, Sylvain Le Gall                                    #
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
