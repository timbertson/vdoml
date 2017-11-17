
{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
	# For development, set OPAM2NIX_DEVEL to your local
	# opam2nix repo path
	devRepo = builtins.getEnv "OPAM2NIX_DEVEL";
	src = fetchgit 	{
		"url" = "https://github.com/timbertson/opam2nix-packages.git";
		"fetchSubmodules" = false;
		"sha256" = "0sbyxjz8k7kw41y5irx5xpjypbn6f34svsgx4rbs7gvlncqfqvw2";
		"rev" = "f1385a7a1ce13a22ec48aef11310c98fc020be44";
	};
	opam2nix = fetchgit 	{
		"url" = "https://github.com/timbertson/opam2nix.git";
		"fetchSubmodules" = false;
		"sha256" = "0cghf7cr6p0c4fs583fxwd3q64q4mb951k9bggnfl98g999d3xhb";
		"rev" = "a81d0781167a5241158d02b0b71add8d11bfeeaa";
	};
in
if devRepo != "" then
	let toPath = s: /. + s; in
	callPackage "${devRepo}/nix" {} {
		src = toPath "${devRepo}/nix/local.tgz";
		opam2nix = let devSrc = "${devRepo}/opam2nix/nix/local.tgz"; in
			if builtins.pathExists devSrc then toPath devSrc else opam2nix;
	}
else callPackage "${src}/nix" {} { inherit src opam2nix; }
