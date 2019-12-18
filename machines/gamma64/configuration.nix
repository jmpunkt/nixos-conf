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
    kernelParams = [
      "acpi_backlight=native"
      "amd_iommu=pt"
      "ivrs_ioapic[32]=00:14.0"
    ];
    initrd.luks.devices."root".allowDiscards = true;
  };

  fileSystems."/".options = ["noatime" "nodiratime" "discard"];

  networking.hostName = "gamma64";

  hardware.cpu.amd.updateMicrocode = true;
  hardware.enableRedistributableFirmware = true;

  services = {
    tlp = {
      enable = true;
      extraConfig = ''
        RUNTIME_PM_BLACKLIST="05:00.3 05:00.4"
      '';
    };
    xserver.libinput = {
      enable = true;
      accelProfile = "flat";
      naturalScrolling = true;
      tapping = true;
    };
  };

  system.stateVersion = "19.09";
}
