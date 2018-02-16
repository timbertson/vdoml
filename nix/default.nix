{ pkgs, src, opam2nix ? pkgs.callPackage ./opam2nix-packages.nix {} }:
with pkgs;
let
	chompFile = file: lib.removeSuffix "\n" (builtins.readFile file);
	opam-installer = callPackage ./opam-installer.nix { inherit opam2nix; };
in
opam2nix.buildOpamPackage {
	name = "vdoml-${chompFile ../VERSION}";
	inherit src;
	ocamlAttr = "ocaml_4_03";
	opamFile = ../vdoml.opam;
	extraPackages = [ "ppx_inline_test" "sexplib" "ppx_assert" ];
	shellHook = ''
		export OCAMLPATH="$OCAMLPATH:$PWD/lib"
	'';
}
