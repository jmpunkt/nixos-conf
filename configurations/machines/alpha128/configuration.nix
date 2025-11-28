{
  config,
  pkgs,
  options,
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
    };
    initrd.systemd.enable = true;
    tmp.cleanOnBoot = true;
    blacklistedKernelModules = [
      # Disable: https://bbs.archlinux.org/viewtopic.php?id=239075
      "sp5100_tco"
    ];
  };
  networking.hostName = "alpha128";
  environment.systemPackages = with pkgs; [
    razergenie
    nvtopPackages.amd
  ];
  system.stateVersion = "25.05";
}
