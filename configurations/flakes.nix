{ lib
, config
, pkgs
, options
, ...
}:
{
  nix = {
    package = pkgs.nixFlakes;
    extraOptions =
      ''
      experimental-features = nix-command flakes
      '';
  };
}
