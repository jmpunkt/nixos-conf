{
  lib,
  config,
  pkgs,
  options,
  ...
}: {
  nix = {
    package = pkgs.nix;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
}
