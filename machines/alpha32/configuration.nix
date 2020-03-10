{ config, pkgs, options, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./../../configurations/kde.nix
    ./../../configurations/development.nix
    ./../../configurations/yubico.nix
    ./../../configurations/users/jonas.nix
  ];

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

  hardware.cpu.amd.updateMicrocode = true;

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
