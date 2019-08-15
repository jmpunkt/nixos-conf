# /bin/sh
# run inside nix-shell -p nodePackages.node2nix

node2nix -8 -i packages.json -c plugins.nix
