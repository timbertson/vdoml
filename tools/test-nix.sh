#!/bin/bash
set -eux

NIX_PIN="$(nix-build --no-out-link '<nixpkgs>' -A 'nix-pin')/bin/nix-pin"
set +x
"$NIX_PIN" create vdoml . --path nix/default.nix
bash <(curl -sSL 'https://gist.githubusercontent.com/timbertson/b619802d795c270f80886cee8efd4191/raw/travis-long-output.sh') \
	"$NIX_PIN" build --show-trace .
echo "== Built files:"
ls -lR result/

