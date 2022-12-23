{
  lib,
  config,
  pkgs,
  options,
  ...
}: {
  imports = import ../modules/all-nixos.nix ++ [./flakes.nix];
  nix.settings = {
    substituters = ["https://nix-community.cachix.org"];
    trusted-public-keys = ["nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="];
  };
  nixpkgs.overlays = [(import ../overlays/00-patch.nix) (import ../overlays/10-pkgs.nix)];
  programs = {
    # Disable command-not-found since there is probably no nixos
    # channel
    command-not-found.enable = lib.mkForce false;
  };
  security.sudo.enable = lib.mkForce false;
}
