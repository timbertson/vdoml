{ pkgs, src }:
with pkgs;
let
	chompFile = file: lib.removeSuffix "\n" (builtins.readFile file);
	opam2nix = callPackage ./opam2nix-packages.nix {};
	opamDeps = ["ppx_test" "ppx_assert"] ++ (lib.splitString "," (chompFile ./opam-dep-names));
	opamConfig = {
		packages = opamDeps;
		ocamlAttr = "ocaml_4_02";
		args= ["--verbose" ];
	};
	makeOpamRepository = {
		name, version, opamFile, src,
		descr ? "anonymous opam package"
	}:
		derivation {
			name = "opam-custom";
			system = builtins.currentSystem;
			builder =
			pkgs.writeScript "opam-builder.sh"
				''#!${pkgs.bash}/bin/bash
					export PATH="${pkgs.coreutils}/bin"
					dest="$out/packages/${name}/${name}.${version}"
					mkdir -p "$dest"
					cd "$dest"
					cp ${opamFile} ./opam
					cat > ./descr <<'END_DESCR'
${descr}
END_DESCR
					if [ -f "${src}" ]; then
						echo 'archive: "${src}"' > url
					else
						echo 'src: "${src}"' > url
					fi
			'';
		};
in
stdenv.mkDerivation {
	name = "vdoml";
	passthru = {
		opamDepNames = opamDeps;
		opamDeps = opam2nix.buildPackageSet opamConfig;
		selections = opam2nix.selectionsFileLax opamConfig;
		opamRepo = makeOpamRepository {
			name = "vdoml";
			version = chompFile ../VERSION;
			opamFile = ../vdoml.opam;
			inherit src;
		};
		inherit (opam2nix) opam2nix;
	};
	inherit src;
	buildInputs = (opam2nix.build opamConfig) ++ [python];
	buildPhase = ''
		tools/bin/gup lib
	'';
	installPhase = ''
		mkdir -p "$out"
		cp -r --dereference lib/vdoml "$out/"
	'';
	shellHook = ''
		export OCAMLPATH="$OCAMLPATH:$PWD/lib"
	'';
}
