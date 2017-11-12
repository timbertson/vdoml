
{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
	# For development, set OPAM2NIX_DEVEL to your local
	# opam2nix repo path
	devRepo = builtins.getEnv "OPAM2NIX_DEVEL";
	src = fetchgit 	{
		"url" = "https://github.com/timbertson/opam2nix-packages.git";
		"fetchSubmodules" = false;
		"sha256" = "0hkspr2dx699wl03iib5y1d43lia7l8p103rflnnp9zgfqyyd3k5";
		"rev" = "4608343789d6a26c72f0861ead1d5d0ed9190e69";
	};
	opam2nix = fetchgit 	{
		"url" = "https://github.com/timbertson/opam2nix.git";
		"fetchSubmodules" = false;
		"sha256" = "0p95hfwrz2np3y4l80jhmpm8l3yr823srwc5c6v7g6918br626s8";
		"rev" = "c44a3e00c2af96a08c1a829bfbd543dbc47c3a90";
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
