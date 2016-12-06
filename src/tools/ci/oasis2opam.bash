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

if [ -z "$LOCAL" ] ; then
  . "$(dirname $0)/opam.bash" || exit 1
  opam install oasis2opam oasis
fi

restore_oasis () {
  if [ -e _oasis.org ] ; then
    mv _oasis.org _oasis
  fi
}

if ! command -v oasis2opam > /dev/null ; then
  echo "E: oasis2opam must be installed to regenerate opam/* files." >&2
  exit 1
fi

# Merge with current master branch.
restore_oasis
cp _oasis _oasis.org
VERSION="$(oasis -ignore-plugins query version)-$(date +'%Y-%m-%d')-${BUILD_NUMBER:-0}"
NAME="$(oasis -ignore-plugins query name)"

# Generate opam files.
# TODO: remove when oasis2opam will support BugReports field/plugin feature.
# sed -i "s/BugReports:/XBugReports:/" _oasis
sed -i "s/AlphaFeatures:/XAlphaFeatures:/" _oasis
sed -i "s/ImplementationPatterns+:/XImplementationPatterns+:/" _oasis
sed -i "s/^Version:.*/Version: ${VERSION}/" _oasis
oasis2opam --local -y
# TODO: allow to define a branch for dev-repo directly in oasis2opam.
sed -i 's,^\(dev-repo:.*oasis.git\),\1#opam/testing,' opam/opam

# Commit changes.
restore_oasis
if [ -z "$LOCAL" ] ; then
  git add opam _oasis_remove_.ml "${NAME}.install"
  git commit  -m "Setup OPAM pinning v${VERSION}."
fi
