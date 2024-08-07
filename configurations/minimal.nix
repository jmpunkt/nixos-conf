{
  lib,
  config,
  pkgs,
  options,
  ...
}: {
  imports = import ../modules/all-nixos.nix ++ [./flakes.nix];
  nixpkgs.overlays = [(import ../overlays/00-patch.nix) (import ../overlays/10-pkgs.nix)];
  programs = {
    # Disable command-not-found since there is probably no NixOS channel
    command-not-found.enable = lib.mkForce false;
  };
  security.sudo.enable = lib.mkForce false;
}
