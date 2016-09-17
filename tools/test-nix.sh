#!/bin/bash
set -eux
if ! which nix-build >/dev/null; then
	. "$(dirname "$0")/install-nix.sh"
fi

tools/bin/gup -u nix/local.tgz

# # first, run a nix-shell to check dependencies
# # (verbose; so we only log it if it fails)
# if ! nix-shell --show-trace --run true >log 2>&1; then
# 	tail -n500 log
# 	exit 1
# fi
#
# # dependencies OK; run a build
set +x
function build {
	nix-build --show-trace
	echo "== Built files:"
	ls -lR result/
}

