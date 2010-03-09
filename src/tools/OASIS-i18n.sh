#!/bin/sh

LOCDIR="$(dirname $(readlink -f $0))"
BUILDDIR="$(readlink -f "$LOCDIR/../../_build")"
exec "$BUILDDIR/src/Main.byte" --gettext-dir "$BUILDDIR/po" "$@"
