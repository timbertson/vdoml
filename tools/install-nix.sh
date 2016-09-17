set -eu
[ "$CI" = true ]
bash <(curl -sS https://gist.githubusercontent.com/timbertson/f643b8ae3a175ba8da7f/raw/travis-nix-bootstrap.sh)
source $HOME/.nix-profile/etc/profile.d/nix.sh
