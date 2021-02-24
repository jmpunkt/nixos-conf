{ config, pkgs, options, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./../../configurations/kde.nix
    ./../../configurations/yubico.nix
    ./../../configurations/users/jonas.nix
    ./../../configurations/games.nix
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
    initrd.kernelModules = [ "amdgpu" ];
  };

  networking.hostName = "alpha128";

  hardware = {
    opengl.extraPackages = with pkgs; [
      rocm-opencl-icd
    ];
  };

  users.users.jonas = {
    extraGroups = [
      config.users.groups.plugdev.name
    ];
  };

  hardware.openrazer = {
    enable = true;
    syncEffectsEnabled = false;
    mouseBatteryNotifier = false;
  };

  environment.systemPackages = with pkgs; [
    razergenie
  ];

  services.xserver.videoDrivers = [ "amdgpu" ];

  system.stateVersion = "20.03";
}
