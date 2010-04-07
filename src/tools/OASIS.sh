#!/bin/sh 

set -e 

ROOTDIR="$(readlink -f $(dirname "$0")/..)"

# Environment variables for OASIS

if [ "x$ROOTDIR" = "x" ]; then
  echo "ROOTDIR not set";
fi

export OASIS_GETTEXT_DIR="$ROOTDIR/share/locale"

exec "$ROOTDIR/libexec/OASIS" "$@"

