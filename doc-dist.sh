#!/bin/sh

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
