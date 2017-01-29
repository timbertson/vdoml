#!/bin/bash
set -eux
if ! which nix-build >/dev/null; then
	. "$(dirname "$0")/install-nix.sh"
fi

tools/bin/gup -u nix/local.tgz

set +x
nix-build --show-trace .
echo "== Built files:"
ls -lR result/

