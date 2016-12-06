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

mkdir dist || true
mkdir tmp || true
. "$(dirname $0)/opam.bash" || exit 1

if [ "x${OPAM_BUILD_REVDEPS_UNSTABLE}" == "xyes" ] ; then
  opam pin add opam-build-revdeps \
    'git://github.com/gildor478/opam-build-revdeps.git#opam/unstable'
else
  opam install opam-build-revdeps
fi

# TODO: un-exclude when upgrading to Debian Stretch
# - maxminddb
# - brotli
# - zstd
opam-build-revdeps compare --package oasis ${OPAM_BUILD_REVDEPS_ARGS} \
  --exclude qfs \
  --exclude maxminddb \
  --exclude brotli \
  --exclude zstd \
  --ocaml_version 4.03.0 \
  --version1 latest \
  --version2 latest --pin2 "oasis:$(pwd)/sut" \
  --html_output "dist/index.html" \
  --css_output "dist/index.css"

