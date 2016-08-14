{ pkgs ? import <nixpkgs> {}}:
with pkgs;
let
	opam2nix = callPackage ./nix/opam2nix-packages.nix {};
	opamDeps = ["eliom" "logs" "ppx_test" "ppx_assert" "sexplib"];
	opamConfig = {
		packages = opamDeps;
		ocamlAttr = "ocaml_4_02";
		args= ["--verbose" ];
		# extraRepos = [ ./opam ];
		overrides = {super, self}: let sels = super.opamSelection; in {
			opamSelection = sels // {
				# TODO: remove ncurses hacks when https://github.com/ocaml/opam-repository/pull/6773 is resolved
				ocamlnet = lib.overrideDerivation sels.ocamlnet (o: {
					nativeBuildInputs = o.nativeBuildInputs ++ [ ncurses ];
				});
				lwt = lib.overrideDerivation sels.lwt (o: {
					nativeBuildInputs = o.nativeBuildInputs ++ [ ncurses ];
				});
				virtual_dom = lib.overrideDerivation sels.virtual_dom (o: {
					unpackCmd = ''tar -xzf $src'';
				});
				eliom = lib.overrideDerivation sels.eliom (o: {
					postPatch = ''
						camlp4_path="$(ocamlfind query camlp4)"
						sed -i -e "s|+camlp4|$camlp4_path|" src/_tags
					'';
				});
				ocsigenserver = lib.overrideDerivation sels.ocsigenserver (o:
					let commandPipe = "/tmp/ocsigenserver_command"; in
					{
					# TODO: this is a bit hacky :/
					patches = [ ./nix/skip_commandpipe.diff ];
					# prePatch = ''
					# 	sed -i -e 's|$root$commandPipe|${commandPipe}|' configure
					# 	sed -i -e '/TEMPROOT.*COMMANDPIPE/d' Makefile
					# '';
					# postInstall = ''
					# 	# Can't be copied to the nix store, since it's a FIFO
					# 	rm ${commandPipe}
					# '';
				});
			};
		};
		pkgs = pkgs // {
			gdbm = lib.overrideDerivation pkgs.gdbm (orig: {
				configureFlags = "--enable-libgdbm-compat";
			});
		};
	};
in
stdenv.mkDerivation {
	name = "vdoml";
	passthru = {
		opamDeps = opam2nix.buildPackageSet opamConfig;
		repo = opam2nix.buildNixRepo ./opam;
		selections = opam2nix.selectionsFileLax opamConfig;
		inherit (opam2nix) opam2nix;
	};
	buildInputs = opam2nix.build opamConfig;
}
