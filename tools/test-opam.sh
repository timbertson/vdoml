#!/bin/bash
set -eux
set -o pipefail
export CI="${CI:-false}"

if [ -z "${IN_NIX_SHELL:-}" ] && which nix-shell >/dev/null 2>&1; then
	echo "Running inside nix-shell..."
	exec env IN_NIX_SHELL=1 nix-shell -p opam --run "$0" "$@"
fi

if ! which opam >/dev/null 2>&1; then
	echo 'Error: `opam` required' >&2
	exit 1
fi

here="$(dirname "$(readlink -f "$0")")"
cd "$here/.."

opam lint vdoml.opam

function cleanup() {
	status="$?"
	if [ "$status" != 0 -a 1 = "${SHELL_ON_ERROR:-}" ]; then
		opam config exec --switch=vdoml-test -- bash -i
	fi
	if [ -n "${tempdir:-}" ]; then
		if [ 1 = "${KEEP_OPAM:-}" ]; then
			echo "NOTE: not cleaning up. To re-use:"
			echo "  export SCRATCH_OPAM=$tempdir"
		else
			rm -rf "$tempdir"
		fi
	fi

	exit "$status"
}
trap "cleanup" EXIT

BASE_SWITCH="4.05.0"
if [ -n "${SCRATCH_OPAM:-}" ]; then
	# OPAMYES=1 opam switch system
	# OPAMYES=1 opam switch remove vdoml-test || true
	OPAMYES=1 opam uninstall vdoml || true
	export OPAMROOT="$SCRATCH_OPAM"
else
	tempdir="$(mktemp -d)"
	export OPAMROOT="$tempdir"
	opam init --no-setup --comp="$BASE_SWITCH"
fi

unset OCAMLPATH OCAMLFIND_DESTDIR

export OPAMYES=1
opam switch list | grep -q vdoml-test || opam switch install vdoml-test --alias-of "$BASE_SWITCH"

opam config exec --switch=vdoml-test -- bash -eux <<"EOF"
	export OPAMYES=1
	opam pin add --kind git --no-action "$(pwd)"
	opam install vdoml
	opam list
	tools/bin/gup examples/all
EOF
