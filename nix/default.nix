{ pkgs ? import <nixpkgs> {}, opam2nix ? pkgs.callPackage ./opam2nix-packages.nix {}, ocamlAttr ? "ocaml-ng.ocamlPackages_4_05.ocaml" }:
with pkgs;
let
	chompFile = file: lib.removeSuffix "\n" (builtins.readFile file);
	opam-installer = callPackage ./opam-installer.nix { inherit opam2nix; };
	src = if lib.isStorePath ../.
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
