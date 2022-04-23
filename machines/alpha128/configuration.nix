{
  config,
  pkgs,
  options,
  ...
}: {
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
    kernelParams = ["processor.max_cstate=1"];
    initrd.kernelModules = ["amdgpu"];
  };
  networking.hostName = "alpha128";
  hardware = {opengl.extraPackages = with pkgs; [rocm-opencl-icd];};
  hardware.openrazer = {
    enable = true;
    syncEffectsEnabled = false;
    mouseBatteryNotifier = false;
    users = [config.users.users.jonas.name];
  };
  environment.systemPackages = with pkgs; [razergenie];
  services.xserver.videoDrivers = ["amdgpu"];
  systemd.services.amdgpu-profile-low = {
    description = "Sets the power profile to low within the AMDGPU driver.";
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      User = "root";
      ExecStart =
        pkgs.writers.writeBash
        "enable-profile"
        ''
          echo "low" > /sys/class/drm/card0/device/power_dpm_force_performance_level
        '';
      ExecStop =
        pkgs.writers.writeBash
        "disable-profile"
        ''
          echo "auto" > /sys/class/drm/card0/device/power_dpm_force_performance_level
        '';
      RemainAfterExit = "yes";
    };
  };
  system.stateVersion = "20.03";
}
