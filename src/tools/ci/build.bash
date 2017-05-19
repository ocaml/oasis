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

. "$(dirname $0)/packages.bash" || exit 1
. "$(dirname $0)/opam.bash" || exit 1
mkdir dist || true
opam install "${OPAM_PKGS[@]}"
export OCAMLRUNPARAM=b
ocaml setup.ml -distclean
ocaml setup.ml -configure \
  --enable-tests \
  --enable-devel \
  --enable-tests-omake \
  ${CONFIGURE_ARGS}
ocaml setup.ml -build
ocaml setup.ml -test ${TEST_ARGS}
