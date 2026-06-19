#!/bin/sh
#
# Build the lazyparse heap image
#

set -e

ROOTDIR="$(dirname "$0")/.."
MAINDIR="$ROOTDIR/src/main"
BINDIR="$(dirname "$0")"

echo 'SMLofNJ.exportFn ("../../bin/lazyparse-heap", fn (_, args) => (case args of [filename] => (Main.run (filename, Main.stripExtension filename); OS.Process.success) | [filename, "-o", output] => (Main.run (filename, output); OS.Process.success) | _ => (Main.usage (); OS.Process.failure)));' \
  | (cd "$MAINDIR" && sml -m sources.cm)

cat > "$BINDIR/lazyparse" <<SCRIPT
#!/bin/sh
exec sml @SMLload="\$(dirname "\$0")/lazyparse-heap" @SMLdebug=/dev/null "\$@"
SCRIPT
chmod a+x "$BINDIR/lazyparse"

echo "built $BINDIR/lazyparse"
