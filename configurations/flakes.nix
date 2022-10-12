{
  lib,
  config,
  pkgs,
  options,
  ...
}: {
  nix = {
    package = pkgs.nixVersions.nix_2_9;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
}
