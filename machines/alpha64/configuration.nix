{ config, pkgs, options, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./../../configurations/base.nix
    ./../../configurations/kde.nix
    ./../../configurations/locale.nix
    ./../../configurations/misc.nix
    ./../../configurations/network.nix
    ./../../configurations/shell.nix
    ./../../configurations/tmux.nix
    ./../../configurations/users/jonas.nix
  ];

  nix.nixPath = options.nix.nixPath.default ++
  [ "nixpkgs-overlays=/etc/nixos/nixos-conf/overlays" ];
  nixpkgs.config.allowUnfree = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.cleanTmpDir = true;

  networking.hostName = "alpha64";

  services.xserver.libinput.enable = true;
  services.xserver.libinput.accelProfile = "flat";

  system.stateVersion = "19.09";
}
