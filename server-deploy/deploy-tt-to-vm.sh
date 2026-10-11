#!/bin/bash
# Old copy - forwards to the one real deploy script in cthulhu-wars-tools, which
# refuses uncommitted code, bumps the version, builds, deploys and verifies.
exec /Users/gremus/Claude-Projects/cthulhu-wars-tools/server-deploy/deploy-tt-to-vm.sh "$@"
