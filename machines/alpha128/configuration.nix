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
    };
    initrd.systemd.enable = true;
    tmp.cleanOnBoot = true;
    blacklistedKernelModules = [
      # Disable: https://bbs.archlinux.org/viewtopic.php?id=239075
      "sp5100_tco"
    ];
  };
  networking.hostName = "alpha128";
  hardware.bluetooth.enable = true;
  hardware.openrazer = {
    enable = true;
    syncEffectsEnabled = false;
    batteryNotifier.enable = false;
    users = [config.users.users.jonas.name];
  };
  environment.systemPackages = with pkgs; [razergenie nvtopPackages.amd];
  system.stateVersion = "20.03";
}
