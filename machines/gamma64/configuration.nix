{
  config,
  pkgs,
  options,
  lib,
  ...
}: {
  imports = [
    ./hardware-configuration.nix
    ./../../configurations/kde.nix
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
    tmp.cleanOnBoot = true;
    kernelParams = [
      "acpi_backlight=native"
      "ivrs_ioapic[32]=00:14.0"
      "intel_pstate=disable"
      "initcall_blacklist=acpi_cpufreq_init"
      "amd_pstate.shared_mem=1"
    ];
    kernelModules = ["amd-pstate"];
    initrd.luks.devices."root".allowDiscards = true;
  };
  fileSystems."/".options = ["noatime" "nodiratime" "discard"];
  networking.hostName = "gamma64";
  hardware.cpu.amd.updateMicrocode = true;
  environment.systemPackages = with pkgs; [tlp powertop s-tui config.boot.kernelPackages.cpupower];
  services = {
    power-profiles-daemon.enable = lib.mkForce false;
    tlp = {
      enable = true;
      settings = {
        "RUNTIME_PM_BLACKLIST" = "05:00.3 05:00.4";
        TLP_DEFAULT_MODE = "BAT";
        CPU_BOOST_ON_AC = 1;
        CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
        CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
        CPU_SCALING_GOVERNOR_ON_AC = "schedutil";
        PCIE_ASPM_ON_BAT = "powersupersave";
      };
    };
    xserver.libinput.touchpad = {
      naturalScrolling = true;
      tapping = true;
    };
  };
  system.stateVersion = "20.03";
}
