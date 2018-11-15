{ pkgs ? import <nixpkgs> {}, opam2nix ? pkgs.callPackage ./opam2nix-packages {}, ocamlAttr ? "ocaml-ng.ocamlPackages_4_06.ocaml" }:
with pkgs;
let
	chompFile = file: lib.removeSuffix "\n" (builtins.readFile file);
	opam-installer = callPackage ./opam-installer.nix { inherit opam2nix; };
	isStorePath = x: lib.isStorePath (builtins.toString x); # workaround https://github.com/NixOS/nixpkgs/issues/48743
	src = if isStorePath ../.
		then ../.
		else (nix-update-source.fetch ./src.json).src;
in
opam2nix.buildOpamPackage {
	name = "vdoml-${chompFile ../VERSION}";
	inherit src ocamlAttr;
	opamFile = ../vdoml.opam;
	extraPackages = [ "ppx_inline_test" "sexplib" "ppx_assert" ];
	shellHook = ''
		export OCAMLPATH="$OCAMLPATH:$PWD/lib"
	'';
}
