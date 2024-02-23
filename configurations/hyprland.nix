{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [./desktop.nix];
  programs.hyprland.enable = true;
  environment.systemPackages = with pkgs; [
  ];
}
