let sources = import ./sources.nix {}; in
{
	pkgs ? import <nixpkgs> {},
	opam2nix ? pkgs.callPackage sources.opam2nix {},
}:
with pkgs;
let
	chompFile = file: lib.removeSuffix "\n" (builtins.readFile file);
	opamPackages = opam2nix.build {
		src = sources.local { url = ../.; };
		selection = ./opam-selection.nix;
		ocaml = ocaml-ng.ocamlPackages_4_14.ocaml;
		override = {}: {
			dune = base: base.overrideAttrs (base: {
				buildInputs = (base.buildInputs or []) ++
				(lib.optionals stdenv.isDarwin (with darwin.apple_sdk;
					[ frameworks.CoreServices ]
				));
			});
		};
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
