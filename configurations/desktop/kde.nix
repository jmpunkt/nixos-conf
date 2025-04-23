{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./non-virtual.nix
    ./kde-minimal.nix
  ];

  environment.systemPackages = with pkgs; [
    simple-scan
    fuzzel
  ];
}
