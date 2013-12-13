#!/bin/sh
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

OASIS_VERSION=$(oasis query version)
CURDIR=$(pwd)

TMPDIR=$(mktemp -d)

clean_exit () {
  if [ -n "$TMPDIR" ] && [ -e "$TMPDIR" ]; then
    rm -rf "$TMPDIR"
  fi
}

trap clean_exit EXIT

mk_tarball () {
  TOPDIR="$1"
  DSTDIR="$2"
  mkdir -p "$TMPDIR/$TOPDIR/doc"
  cp doc/*.mkd "$TMPDIR/$TOPDIR/doc"

  mkdir -p "$TMPDIR/$TOPDIR/api-oasis"
  cp -R _build/src/api-oasis.docdir/* "$TMPDIR/$TOPDIR/api-oasis"

  tar czf "$DSTDIR/$TOPDIR.tar.gz" -C $TMPDIR $TOPDIR
}

mk_tarball "oasis-doc-dev" "$CURDIR/dist"
mk_tarball "oasis-doc-$OASIS_VERSION" "$CURDIR/dist"
