{
  lib,
  config,
  pkgs,
  options,
  ...
}:
{
  imports = import ../modules/all-nixos.nix;
  nixpkgs.overlays = [
    (import ../overlays/00-patch.nix)
    (import ../overlays/10-pkgs.nix)
  ];

  # Disable command-not-found since there is probably no NixOS channel
  programs.command-not-found.enable = lib.mkForce false;

  # use doas instead of sudo
  security.sudo.enable = lib.mkForce false;
  security.doas = {
    enable = true;
    extraRules = [
      {
        groups = [ "wheel" ];
        persist = true;
        keepEnv = true;
      }
    ];
  };

  # enable flakes
  nix = {
    package = pkgs.nixVersions.latest;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  networking.firewall.enable = true;
  services.timesyncd.enable = lib.mkForce true;
  programs.fish.enable = true;
}
