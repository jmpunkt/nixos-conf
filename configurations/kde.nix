{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [./desktop.nix ./kde-minimal.nix];

  environment.systemPackages = with pkgs; [
    simple-scan
    fuzzel
  ];
}
