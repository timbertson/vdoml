
{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
	# For development, set OPAM2NIX_DEVEL to your local
	# opam2nix repo path
	devRepo = builtins.getEnv "OPAM2NIX_DEVEL";
	src = fetchgit 	{
		"url" = "https://github.com/timbertson/opam2nix-packages.git";
		"fetchSubmodules" = false;
		"sha256" = "0xdxpp50rnmvnryll4acmd0p0p300ky71qm4gi4dljsb1mdp9j6z";
		"rev" = "a8ddd0d052415e788e8e6292beba295191e9d5fb";
	};
	opam2nix = fetchgit 	{
		"url" = "https://github.com/timbertson/opam2nix.git";
		"fetchSubmodules" = false;
		"sha256" = "1sr0fnw8m071g3h6phr21jm6568gs9y3ll3izypb3hvacgc6x3vr";
		"rev" = "dca65798560f75ee76a89e75255d81107609314d";
	};
in
if devRepo != "" then
	let toPath = s: /. + s; in
	callPackage "${devRepo}/nix" {} {
		src = toPath "${devRepo}/nix/local.tgz";
		opam2nix = toPath "${devRepo}/opam2nix/nix/local.tgz";
	}
else callPackage "${src}/nix" {} { inherit src opam2nix; }
