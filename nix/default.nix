{
	pkgs ? import <nixpkgs> {},
	opam2nix,
	ocamlAttr ? "ocaml-ng.ocamlPackages_4_06.ocaml",
	self,
}:
with pkgs;
let
	chompFile = file: lib.removeSuffix "\n" (builtins.readFile file);
	opamPackages = opam2nix.build {
		src = self;
		selection = ./opam-selection.nix;
		ocaml = ocaml-ng.ocamlPackages_4_08.ocaml;
	};
	result = {
		inherit opam2nix;
		inherit (opamPackages) vdoml;
		shell = with opamPackages; vdoml.overrideAttrs (o: {
			buildInputs = (o.buildInputs or [])
				++ [ ppx_inline_test sexplib ppx_assert ];
		});
	};
in
result.vdoml.overrideAttrs (_: { passthru = result; })
