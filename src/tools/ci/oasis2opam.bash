. "$(dirname $0)/opam.bash" || exit 1
opam install oasis2opam oasis

restore_oasis () {
  git checkout _oasis
}

# Merge with current master branch.
restore_oasis
VERSION="$(oasis -ignore-plugins query version)-$(date +'%Y-%m-%d')-${BUILD_NUMBER:-0}"
NAME="$(oasis -ignore-plugins query name)"

# Generate opam files.
# TODO: remove when oasis2opam will support BugReports field/plugin feature.
sed -i "s/BugReports:/XBugReports:/" _oasis
sed -i "s/AlphaFeatures:/XAlphaFeatures:/" _oasis
sed -i "s/ImplementationPatterns+:/XImplementationPatterns+:/" _oasis
sed -i "s/^Version:.*/Version: ${VERSION}/" _oasis
oasis2opam --local -y
# TODO: allow to define a branch for dev-repo directly in oasis2opam.
sed -i 's,^\(dev-repo:.*oasis.git\),\1#opam/testing,' opam/opam

# Commit changes.
restore_oasis
git add opam _oasis_remove_.ml "${NAME}.install"
git commit  -m "Setup OPAM pinning v${VERSION}."
