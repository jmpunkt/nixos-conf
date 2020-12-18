{ config, pkgs, options, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./../../configurations/kde.nix
    ./../../configurations/development.nix
    ./../../configurations/yubico.nix
    ./../../configurations/users/jonas.nix
    ./../../configurations/razer.nix
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
    cpu.amd.updateMicrocode = true;
    opengl.extraPackages = with pkgs; [
      rocm-opencl-icd
      amdvlk
    ];
  };

  services.xserver.videoDrivers = [ "amdgpu" ];

  system.stateVersion = "20.03";
}
