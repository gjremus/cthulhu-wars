#!/bin/bash
# Deploy Bubastis (BB) build to the live VM at cwo.freeddns.org/BB/.
#
# Mirrors the TchoTcho deploy pattern (deploy-tt-to-vm.sh in the TT project).
# VM tree: /opt/cwo/bb/  (index.html, webp/, fonts/, target/)
#
# Usage:
#   ./deploy-bb-to-vm.sh               # main.js + index.html only (fast)
#   ./deploy-bb-to-vm.sh --assets      # also resync webp + fonts (slow, ~50MB)
#   ./deploy-bb-to-vm.sh --build       # force `sbt fullLinkJS` before deploying
#
# The shared akka-http server JAR lives in the TchoTcho online/ folder.
# Rebuild it there and redeploy when adding new build routes.
# This script only pushes BB static assets.

set -euo pipefail

BB_ROOT="/Users/gremus/Claude-Projects/cthulhu-wars-Bubastis"
SSH_KEY="/Users/gremus/Library/CloudStorage/GoogleDrive-gremus@salesforce.com/My Drive/Personal/Games/Cthulhu Wars/Library at Celaeno/Server Deployment/oracle_cw_ed25519"
HOST="oracle-cw-server@35.255.125.91"
REMOTE_ROOT="/opt/cwo/bb"

DO_BUILD=false
DO_ASSETS=false
for arg in "$@"; do
    case "$arg" in
        --build)  DO_BUILD=true ;;
        --assets) DO_ASSETS=true ;;
        -h|--help) sed -n '1,40p' "$0"; exit 0 ;;
        *) echo "unknown arg: $arg"; exit 2 ;;
    esac
done

if [ ! -f "$SSH_KEY" ]; then
    echo "ERROR: ssh key not found at: $SSH_KEY"
    exit 1
fi

MAIN_JS="$BB_ROOT/solo/target/scala-2.13/cthulhu-wars-solo-hrf-opt/main.js"
if $DO_BUILD || [ ! -f "$MAIN_JS" ]; then
    echo "==> [build] sbt fullOptJS (Bubastis) ..."
    export JAVA_HOME=/Users/gremus/.local/jdk/zulu21.50.19-ca-jdk21.0.11-macosx_aarch64/Contents/Home
    export PATH="$JAVA_HOME/bin:$HOME/.local/bin:$PATH"
    (cd "$BB_ROOT/solo" && sbt fullOptJS 2>&1 | tail -5)
fi
if [ ! -f "$MAIN_JS" ]; then
    echo "ERROR: $MAIN_JS still missing after build."
    exit 1
fi

CACHE_TAG="$(date +%Y%m%d-%H%M%S)"
echo "==> [stage] cache tag = $CACHE_TAG"

# ── LOCAL STAGE (localhost:999/BB/ via TT online server) ───────────────────
LOCAL_BB_DIR="/Users/gremus/Claude-Projects/cthulhu-wars-TchoTcho_Cats_Yuggoth/bb"
LOCAL_INDEX="$LOCAL_BB_DIR/index.html"
echo "==> [local] write patched index.html → $LOCAL_INDEX"
mkdir -p "$LOCAL_BB_DIR"
cp "$BB_ROOT/solo/index.html" "$LOCAL_INDEX"
sed -i '' \
    -e 's|###SERVER-URL###|http://localhost:999/BB/|g' \
    -e "s|main\\.js?v=[A-Za-z0-9-]*|main.js?v=$CACHE_TAG|g" \
    "$LOCAL_INDEX"

# ── REMOTE STAGE (https://cwo.freeddns.org/BB/) ────────────────────────────
TMP_INDEX="$(mktemp -t bb-index.XXXXXX).html"
cp "$BB_ROOT/solo/index.html" "$TMP_INDEX"
sed -i '' \
    -e 's|###SERVER-URL###|https://cwo.freeddns.org/BB/|g' \
    -e "s|main\\.js?v=[A-Za-z0-9-]*|main.js?v=$CACHE_TAG|g" \
    "$TMP_INDEX"

echo "==> [remote] ensure /opt/cwo/bb/target tree exists ..."
ssh -i "$SSH_KEY" "$HOST" "mkdir -p $REMOTE_ROOT/target/scala-2.13/cthulhu-wars-solo-hrf-opt"

echo "==> [upload] main.js → $REMOTE_ROOT/target/scala-2.13/.../main.js"
scp -i "$SSH_KEY" -C "$MAIN_JS" \
    "$HOST:$REMOTE_ROOT/target/scala-2.13/cthulhu-wars-solo-hrf-opt/main.js"

echo "==> [upload] index.html → $REMOTE_ROOT/index.html"
scp -i "$SSH_KEY" -C "$TMP_INDEX" "$HOST:$REMOTE_ROOT/index.html"
rm -f "$TMP_INDEX"

if $DO_ASSETS; then
    echo "==> [upload] webp + fonts (tar pipe) ..."
    (cd "$BB_ROOT/solo" && tar -czf - webp fonts \
        | ssh -i "$SSH_KEY" -C "$HOST" "cd $REMOTE_ROOT && tar -xzf -")
fi

echo "==> [verify] HEAD /BB/ (public)"
curl -s -o /dev/null -w "  public HTTP %{http_code}  size_bytes=%{size_download}\n" "https://cwo.freeddns.org/BB/" || true

echo "==> [verify] HEAD /BB/ (local)"
curl -s -o /dev/null -w "  local  HTTP %{http_code}  size_bytes=%{size_download}\n" "http://localhost:999/BB/" || true

echo
echo "Done.  Public: https://cwo.freeddns.org/BB/   Local: http://localhost:999/BB/   (cache tag $CACHE_TAG)"
echo "Allow ~1 min for browsers with warm caches to refetch main.js."
echo "If localhost:999/BB/ returns 404, the local online server hasn't picked up the BB route yet — restart it."
