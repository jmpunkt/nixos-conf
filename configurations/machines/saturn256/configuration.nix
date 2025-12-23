# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  nixpkgs.config.allowUnfree = true;

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    initrd.systemd.enable = true;
    tmp.cleanOnBoot = true;
    # iommu=pt
  };

  boot.extraModulePackages = [
    pkgs.linuxPackages_latest.jmpunkt.ec_su_axb35
  ];
  boot.kernelModules = [
    "ec_su_axb35"
    "ec_su_axb35_hwmon" # `sensors` will report the fan RPM
  ];

  disko.devices = {
    disk = {
      main = {
        device = "/dev/nvme0n1p1";
      };
    };
  };

  profiles = {
    desktop.kde.enable = true;
    yubikey.enable = true;
    games.enable = true;
    development.enable = true;
    locales.germany.enable = true;
  };
  networking.hostName = "saturn256";
  hardware.amdgpu = {
    initrd.enable = true;
    opencl.enable = true;
  };
  environment.systemPackages = with pkgs; [
    razergenie
    ollama-vulkan
    nvtopPackages.amd
  ];
  system.stateVersion = "25.11";
}
