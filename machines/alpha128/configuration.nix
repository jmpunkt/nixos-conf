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
    blacklistedKernelModules = [
      # Disable: https://bbs.archlinux.org/viewtopic.php?id=239075
      "sp5100_tco"
    ];
    kernelParams = [ "processor.max_cstate=1" ];
  };

  networking.hostName = "alpha128";

  hardware.cpu.amd.updateMicrocode = true;

  services.xserver.libinput = {
    enable = true;
    accelProfile = "flat";
  };

  system.stateVersion = "19.09";
}
