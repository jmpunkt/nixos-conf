({ modulesPath, config, pkgs, ... }: {

  imports = [
    "${modulesPath}/installer/cd-dvd/channel.nix"
    "${modulesPath}/installer/cd-dvd/installation-cd-graphical-base.nix"
  ];

  isoImage.volumeID =
    "nixos-${config.system.nixos.release}-${pkgs.stdenv.hostPlatform.uname.processor}";

  environment.systemPackages = with pkgs; [
    chromium
    gparted
    neovim
    alacritty
  ];

  programs.fish.enable = true;

  services.xserver = {
    desktopManager.xfce.enable = true;
    displayManager = {
      lightdm = {
        enable = true;
        autoLogin = {
          enable = true;
          user = "nixos";
        };
      };
    };
  };

  services.chrony.enable = true;

  time.timeZone = "Europe/Berlin";
  boot.kernelPackages = pkgs.linuxPackages_latest;
  services.xserver.layout = "de";
  console.keyMap = "de";
})
