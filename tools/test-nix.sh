#!/bin/bash
set -eux

set +x
bash <(curl -sSL 'https://gist.githubusercontent.com/timbertson/b619802d795c270f80886cee8efd4191/raw/travis-long-output.sh') \
	nix-build --show-trace -A vdoml
echo "== Built files:"
ls -lR result/

