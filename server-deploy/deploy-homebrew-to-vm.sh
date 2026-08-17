#!/bin/bash
# Deploy Homebrew (HB) build to the live VM at cwo.freeddns.org/HB/.
#
# Mirrors deploy-bb-to-vm.sh (BB pattern) for the Homebrew worktree.
# VM tree: /opt/cwo/HB/  (index.html, webp/, fonts/, target/)
#
# Usage:
#   ./deploy-homebrew-to-vm.sh               # auto-(re)build if stale, then deploy
#   ./deploy-homebrew-to-vm.sh --assets      # also resync webp + fonts
#   ./deploy-homebrew-to-vm.sh --build       # force `sbt fullOptJS` before deploying
#
# FRESHNESS GUARANTEE (added 2026-08-15 after a stale-bundle incident):
#   A plain deploy used to reuse whatever main.js already sat in target/, even if
#   it predated the source changes — so a real fix never reached players while the
#   deploy still said "success / HTTP 200". This script now GUARANTEES freshness:
#     1. It rebuilds automatically whenever any source file is newer than the
#        bundle (not just when --build is passed).
#     2. It builds with a 4G heap (the default 1G OOMs mid-optimize and silently
#        leaves the OLD bundle in place), deletes the old bundle first, and asserts
#        the build actually regenerated it AND the sbt log shows [success] +
#        "Closure: 0 error(s)" with no OutOfMemoryError.
#     3. After upload it sha256-compares the LIVE main.js to the local build and
#        FAILS if they differ. HTTP 200 alone is never accepted as proof.
#   NOTE: do not run two builds against the SAME worktree at once (they share
#   target/). Build/deploy one worktree at a time.

set -euo pipefail

HB_ROOT="/Users/gremus/Claude-Projects/cw-homebrew-wt"
SSH_KEY="/Users/gremus/.ssh/oracle_cw_ed25519"
HOST="oracle-cw-server@cwo.freeddns.org"
REMOTE_ROOT="/opt/cwo/HB"
PUBLIC_BASE="https://cwo.freeddns.org/HB"
OPT_DIR="target/scala-2.13/cthulhu-wars-solo-hrf-opt"

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

MAIN_JS="$HB_ROOT/solo/$OPT_DIR/main.js"

# ── Decide whether a (re)build is required ─────────────────────────────────
# Rebuild if forced, if the bundle is missing, or if ANY source file is newer
# than the current bundle (the bundle is stale relative to source).
NEED_BUILD=false
if $DO_BUILD; then
    NEED_BUILD=true
elif [ ! -f "$MAIN_JS" ]; then
    NEED_BUILD=true
elif [ -n "$(find "$HB_ROOT/solo" \( -name '*.scala' -o -name '*.sbt' -o -name 'index.html' \) -newer "$MAIN_JS" -print -quit 2>/dev/null)" ]; then
    echo "==> [freshness] a source file is newer than the bundle — forcing rebuild"
    NEED_BUILD=true
fi

if $NEED_BUILD; then
    echo "==> [build] sbt fullOptJS (Homebrew) with 4G heap ..."
    export JAVA_HOME=/Users/gremus/.local/jdk/zulu21.50.19-ca-jdk21.0.11-macosx_aarch64/Contents/Home
    export PATH="$JAVA_HOME/bin:$PATH"
    export SBT_OPTS="-Xmx4g -Xss8m"
    # Delete the old bundle so its continued existence can't masquerade as a
    # successful build (a mid-build OOM would otherwise leave the stale file).
    rm -f "$MAIN_JS"
    BUILD_LOG="$(mktemp -t hb-build.XXXXXX).log"
    if ! ( cd "$HB_ROOT/solo" && sbt -J-Xmx4g fullOptJS ) >"$BUILD_LOG" 2>&1; then
        echo "ERROR: sbt exited non-zero. Tail:"; tail -30 "$BUILD_LOG"; exit 1
    fi
    if grep -q "OutOfMemoryError" "$BUILD_LOG"; then
        echo "ERROR: build hit OutOfMemoryError — bundle NOT fresh. Tail:"; tail -20 "$BUILD_LOG"; exit 1
    fi
    if ! grep -q "\[success\]" "$BUILD_LOG"; then
        echo "ERROR: sbt did not report [success]. Tail:"; tail -20 "$BUILD_LOG"; exit 1
    fi
    if ! grep -q "Closure: 0 error" "$BUILD_LOG"; then
        echo "ERROR: Closure optimizer reported errors/warnings. Tail:"; tail -20 "$BUILD_LOG"; exit 1
    fi
    echo "==> [build] OK — $(grep -m1 'Total time' "$BUILD_LOG")"
    rm -f "$BUILD_LOG"
fi
if [ ! -f "$MAIN_JS" ]; then
    echo "ERROR: $MAIN_JS still missing after build."
    exit 1
fi

CACHE_TAG="$(date +%Y%m%d-%H%M%S)"
echo "==> [stage] cache tag = $CACHE_TAG"

# ── REMOTE STAGE (https://cwo.freeddns.org/HB/) ────────────────────────────
TMP_INDEX="$(mktemp -t hb-index.XXXXXX).html"
cp "$HB_ROOT/solo/index.html" "$TMP_INDEX"
sed -i '' \
    -e 's|###SERVER-URL###|https://cwo.freeddns.org/HB/|g' \
    -e "s|main\\.js?v=[A-Za-z0-9-]*|main.js?v=$CACHE_TAG|g" \
    -e 's|<head>|<head>\n        <base href="/HB/" />|' \
    "$TMP_INDEX"

echo "==> [remote] ensure $REMOTE_ROOT/target tree exists ..."
ssh -i "$SSH_KEY" "$HOST" "mkdir -p $REMOTE_ROOT/$OPT_DIR"

echo "==> [upload] main.js → $REMOTE_ROOT/$OPT_DIR/main.js"
scp -i "$SSH_KEY" -C "$MAIN_JS" \
    "$HOST:$REMOTE_ROOT/$OPT_DIR/main.js"

echo "==> [upload] index.html → $REMOTE_ROOT/index.html"
scp -i "$SSH_KEY" -C "$TMP_INDEX" "$HOST:$REMOTE_ROOT/index.html"
rm -f "$TMP_INDEX"

if $DO_ASSETS; then
    echo "==> [upload] webp + fonts (tar pipe) ..."
    (cd "$HB_ROOT/solo" && tar -czf - webp fonts \
        | ssh -i "$SSH_KEY" -C "$HOST" "cd $REMOTE_ROOT && tar -xzf -")
fi

# ── POST-DEPLOY CONTENT VERIFICATION ───────────────────────────────────────
# HTTP 200 does NOT prove the live bundle is the one we just built. Compare the
# sha256 of the live main.js (cache-busted) to the local build; mismatch = the
# server is serving a stale/other bundle → fail loudly (non-zero exit).
LIVE_URL="$PUBLIC_BASE/$OPT_DIR/main.js"
LOCAL_SHA="$(shasum -a 256 "$MAIN_JS" | awk '{print $1}')"
LIVE_SHA="$(curl -s "$LIVE_URL?cb=$CACHE_TAG" | shasum -a 256 | awk '{print $1}')"
echo "==> [verify] local sha256 = $LOCAL_SHA"
echo "==> [verify] live  sha256 = $LIVE_SHA"
if [ "$LOCAL_SHA" != "$LIVE_SHA" ]; then
    echo "ERROR: live main.js does NOT match the built bundle — the deploy did not take."
    echo "       (server is serving a stale or different bundle). Marking deploy FAILED."
    exit 1
fi
echo "==> [verify] live bundle byte-for-byte matches local build ✓"

echo "==> [verify] HEAD /HB/ (public)"
curl -s -o /dev/null -w "  public HTTP %{http_code}  size_bytes=%{size_download}\n" "https://cwo.freeddns.org/HB/" || true

echo
echo "Done.  Public: https://cwo.freeddns.org/HB/   (cache tag $CACHE_TAG)"
echo "Live bundle verified fresh (sha256 match).  Allow ~1 min for warm browser caches to refetch."
