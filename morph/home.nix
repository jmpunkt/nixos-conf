let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/e5bd0cfcd530f261670dfc7ea54cf416841179e5.tar.gz";
    sha256 = "1vj3bwljkh55si4qjx52zgw7nfy6mnf324xf1l2i5qffxlh7qxb6";
  }) {};
in
{
  network =  {
    inherit pkgs;
    description = "simple hosts";
  };

  "192.168.178.3" = { config, pkgs, options, ... }: {
    imports = [ ../machines/beta32/configuration.nix ];
  };
}
