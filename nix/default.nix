{
	pkgs ? import <nixpkgs> {},
	opam2nix,
	ocamlAttr ? "ocaml-ng.ocamlPackages_4_06.ocaml",
	nix-wrangle,
	self,
}:
with pkgs;
let
	chompFile = file: lib.removeSuffix "\n" (builtins.readFile file);
	opamPackages = opam2nix.build {
		src = self;
		deps = ../opam-packages.nix;
		ocaml = ocaml-ng.ocamlPackages_4_08.ocaml;
	};
in
{
	inherit opam2nix;
	inherit (opamPackages) vdoml;
	shell = with opamPackages; vdoml.overrideAttrs (o: {
		buildInputs = (o.buildInputs or [])
			++ [ ppx_inline_test sexplib ppx_assert ];
		shellHook = ''
			export OCAMLPATH="$OCAMLPATH:$PWD/lib"
		'';
	});
}
