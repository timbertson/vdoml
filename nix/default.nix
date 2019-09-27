{
	pkgs ? import <nixpkgs> {},
	opam2nix,
	ocamlAttr ? "ocaml-ng.ocamlPackages_4_06.ocaml",
	self ? ../.
}:
with pkgs;
let
	chompFile = file: lib.removeSuffix "\n" (builtins.readFile file);
in
opam2nix.buildOpamPackage {
	name = "vdoml-${chompFile ../VERSION}";
	src = self;
	inherit ocamlAttr;
	opamFile = ../vdoml.opam;
	extraPackages = [ "ppx_inline_test" "sexplib" "ppx_assert" ];
	shellHook = ''
		export OCAMLPATH="$OCAMLPATH:$PWD/lib"
	'';
}
