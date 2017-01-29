{ pkgs, src, opam2nix ? pkgs.callPackage ./opam2nix-packages.nix {} }:
with pkgs;
let
	chompFile = file: lib.removeSuffix "\n" (builtins.readFile file);
in
opam2nix.buildOpamPackage {
	name = "vdoml-${chompFile ../VERSION}";
	inherit src;
	ocamlAttr = "ocaml_4_03";
	shellHook = ''
		export OCAMLPATH="$OCAMLPATH:$PWD/lib"
	'';
}
