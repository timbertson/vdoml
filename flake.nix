{
	description = "vdoml";

	inputs = {
		opam2nix = {
			type= "git";
			path = ../opam2nix;
		};
	};
	outputs = { self, nixpkgs }:
	with nixpkgs.legacyPackages.x86_64-linux;
	{
		vdoml = callPackage ./nix { inherit self; };
	});

	edition=201909;
}

