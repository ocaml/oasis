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

OPAM_PKGS="base-bytes base-unix ocamlfind pcre expect fileutils ounit ocamlify
 ocamlmod omake benchmark"

export OPAMYES=1
if [ -f "$HOME/.opam/config" ]; then
    opam update
    opam upgrade
else
    opam init
fi
if [ -n "${OPAM_SWITCH}" ]; then
    opam switch ${OPAM_SWITCH}
fi
eval `opam config env`

opam install $OPAM_PKGS

export OCAMLRUNPARAM=b

ocaml setup.ml -configure --enable-tests --enable-devel
ocaml setup.ml -build
ocaml setup.ml -test
