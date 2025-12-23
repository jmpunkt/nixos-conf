{
  config,
  pkgs,
  options,
  lib,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
  ];

  nixpkgs.config.allowUnfree = true;

  profiles = {
    desktop.kde.enable = true;
    yubikey.enable = true;
    games.enable = true;
    development.enable = true;
    locales.germany.enable = true;
  };

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
      efi.efiSysMountPoint = "/boot/efi";
    };
    tmp.cleanOnBoot = true;
    kernelParams = [
      "ivrs_ioapic[32]=00:14.0"
      "intel_pstate=disable"
      "initcall_blacklist=acpi_cpufreq_init"
      "amd_pstate.shared_mem=1"
    ];
    kernelModules = [ "amd-pstate" ];
    initrd.luks.devices."root".allowDiscards = true;
  };
  fileSystems."/".options = [
    "noatime"
    "nodiratime"
    "discard"
  ];
  networking.hostName = "gamma64";
  hardware.cpu.amd.updateMicrocode = true;
  environment.systemPackages = with pkgs; [
    powertop
    s-tui
    config.boot.kernelPackages.cpupower
  ];
  services.upower.enable = true;
  services.tuned.enable = true;
  services.tlp.enable = lib.mkForce false;
  system.stateVersion = "25.05";
}
