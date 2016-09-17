
{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
	# For development, set OPAM2NIX_DEVEL to your local
	# opam2nix repo path
	devRepo = builtins.getEnv "OPAM2NIX_DEVEL";
	src = fetchgit {
  "url" = "https://github.com/timbertson/opam2nix-packages.git";
  "fetchSubmodules" = false;
  "sha256" = "0jqf94df3l7xij8jdq1gyvrzr2mzp3f2dyv5w27ai5x44nhvb6c4";
  "rev" = "800c198756a6c8c7d7c1539b5eb170bc1afe80f5";
};
	opam2nix = fetchgit {
  "url" = "https://github.com/timbertson/opam2nix.git";
  "fetchSubmodules" = false;
  "sha256" = "1swahnb3wvhd3xvphs4pqh1rz30d1h3nzpg5i3nwkcyd1af8b890";
  "rev" = "b2b554ef3e42cd367922d2ba910966a2ea2bbc98";
};
in
if devRepo != "" then
	let toPath = s: /. + s; in
	callPackage "${devRepo}/nix" {} {
			src = toPath "${devRepo}/nix/local.tgz";
			opam2nix = toPath "${devRepo}/opam2nix/nix/local.tgz";
		}
else callPackage "${src}/nix" {} { inherit src opam2nix; }
