{
  lib,
  config,
  pkgs,
  options,
  inputs,
  ...
}:
{
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

  nix.package = pkgs.nixVersions.latest;

  networking.firewall.enable = true;
  services.timesyncd.enable = lib.mkForce true;
  programs.fish.enable = true;
}
