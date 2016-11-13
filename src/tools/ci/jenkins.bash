DISTDIR="$(pwd)/dist"
export OUNIT_OUTPUT_HTML_DIR="$DISTDIR/ounit-log-\$(suite_name).html"
export OUNIT_OUTPUT_JUNIT_FILE="$DISTDIR/junit-\$(suite_name).xml"
export OUNIT_OUTPUT_FILE="$DISTDIR/ounit-log-\$(suite_name)-\$(shard_id).txt"

# Build and test.
. $(dirname "$0")/build.bash || exit 1
# Create documentation package.
make doc-dist
# Create dev tarball.
make dist-dev

