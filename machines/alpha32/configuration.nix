{ config, pkgs, options, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./../../configurations/kde.nix
    ./../../configurations/development.nix
    ./../../configurations/yubico.nix
    ./../../configurations/users/jonas.nix
  ];

  nix.nixPath = options.nix.nixPath.default ++
  [ "nixpkgs-overlays=/etc/nixos/nixos-conf/overlays" ];
  nixpkgs.config.allowUnfree = true;

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      efi.efiSysMountPoint = "/boot/efi";
    };
    cleanTmpDir = true;
    initrd.luks.devices."enc-root".allowDiscards = true;
  };

  fileSystems."/".options = ["noatime" "nodiratime" "discard"];

  networking.hostName = "alpha32";

  services = {
    tlp.enable = true;
    xserver.libinput = {
      enable = true;
      accelProfile = "flat";
      naturalScrolling = true;
      tapping = true;
    };
  };

  system.stateVersion = "19.09";
}
