#!/bin/bash
# Publish the TchoTcho solo build to gh-pages branch under the /TchoTcho/ subfolder.
#
# CRITICAL: This script ONLY adds/updates /TchoTcho/ within gh-pages.
# It NEVER touches the root index.html, root main.js, webp/, fonts/ at root,
# or the /mnu/ subfolder. Those belong to the Library and MNU builds respectively.
#
# Usage: ./publish-tt-pages.sh [--build]
#   --build  Force sbt fullLinkJS before publishing (default: use existing main.js)

set -euo pipefail

TT_ROOT="/Users/gremus/Claude-Projects/cthulhu-wars-TchoTcho_Cats_Yuggoth"
PAT_FILE="/Users/gremus/Google Drive/My Drive/Personal/Games/Cthulhu Wars/github-pat"
FORK_URL="https://$(cat "$PAT_FILE")@github.com/gjremus/cthulhu-wars.git"
TMP_DIR="/tmp/cw-tt-pages-publish"
CACHE_TAG="$(date +%Y%m%d-%H%M%S)"

DO_BUILD=false
for arg in "$@"; do
    case "$arg" in
        --build) DO_BUILD=true ;;
        *) echo "unknown arg: $arg"; exit 2 ;;
    esac
done

MAIN_JS="$TT_ROOT/solo/target/scala-2.13/cthulhu-wars-solo-hrf-opt/main.js"
if $DO_BUILD || [ ! -f "$MAIN_JS" ]; then
    echo "==> [build] sbt fullLinkJS (TchoTcho) ..."
    JAVA_HOME=/tmp/zulu-jdk/zulu21.50.19-ca-jdk21.0.11-macosx_aarch64/Contents/Home
    export JAVA_HOME
    (cd "$TT_ROOT/solo" && java -jar /tmp/sbt/bin/sbt-launch.jar fullOptJS 2>&1 | tail -5)
fi

echo "==> [clone] gh-pages branch ..."
rm -rf "$TMP_DIR"
git clone --quiet --branch gh-pages "$FORK_URL" "$TMP_DIR" 2>&1 | tail -1

echo "==> [stage] TchoTcho subfolder (cache tag $CACHE_TAG) ..."
mkdir -p "$TMP_DIR/TchoTcho/target/scala-2.13/cthulhu-wars-solo-hrf-opt"
cp "$MAIN_JS" "$TMP_DIR/TchoTcho/target/scala-2.13/cthulhu-wars-solo-hrf-opt/main.js"
rm -rf "$TMP_DIR/TchoTcho/webp" "$TMP_DIR/TchoTcho/fonts"
cp -R "$TT_ROOT/solo/webp"  "$TMP_DIR/TchoTcho/webp"
cp -R "$TT_ROOT/solo/fonts" "$TMP_DIR/TchoTcho/fonts"

# Patch index.html: point server URL at GitHub Pages (no online server), offline mode
cp "$TT_ROOT/solo/index.html" "$TMP_DIR/TchoTcho/index.html"
sed -i '' \
  -e 's|###SERVER-URL###||g' \
  -e 's|data-online="true"|data-online="false"|g' \
  -e 's|./target/scala-2.13/cthulhu-wars-solo-hrf-opt/main.js|./target/scala-2.13/cthulhu-wars-solo-hrf-opt/main.js|g' \
  -e "s|main\\.js?v=[A-Za-z0-9-]*|main.js?v=$CACHE_TAG|g" \
  "$TMP_DIR/TchoTcho/index.html"

# Verify we have NOT touched root or mnu — safety check
if [ ! -f "$TMP_DIR/index.html" ]; then
    echo "WARNING: root index.html missing from gh-pages (unexpected). Aborting to be safe."
    exit 1
fi

echo "==> [commit] add/update TchoTcho subfolder only ..."
cd "$TMP_DIR"
git -c user.email=gjremus@gmail.com -c user.name="George Remus" add TchoTcho/
git -c user.email=gjremus@gmail.com -c user.name="George Remus" commit -q -m "TchoTcho Pages build $CACHE_TAG" || echo "(no changes to commit)"
git push origin gh-pages 2>&1 | tail -3

echo "==> [cleanup]"
rm -rf "$TMP_DIR"

echo
echo "Done.  Live: https://gjremus.github.io/cthulhu-wars/TchoTcho/"
echo "Cache tag: $CACHE_TAG"
echo "Allow 1-2 min for GitHub Pages to redeploy."
