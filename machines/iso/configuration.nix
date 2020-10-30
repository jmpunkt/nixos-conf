{ modulesPath, config, lib, pkgs, ... }:

{
  imports = [
    "${modulesPath}/installer/cd-dvd/channel.nix"
    "${modulesPath}/installer/cd-dvd/installation-cd-base.nix"
  ];

  isoImage.volumeID =
    "nixos-${config.system.nixos.release}-${pkgs.stdenv.hostPlatform.uname.processor}";

  environment.systemPackages = with pkgs; [
    ungoogled-chromium
    gparted
    neovim
    alacritty
  ];

  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
      if (subject.isInGroup("wheel")) {
        return polkit.Result.YES;
      }
    });
  '';

  networking.networkmanager.enable = true;
  networking.wireless.enable = lib.mkForce false;

  powerManagement.enable = true;

  hardware.pulseaudio.enable = true;

  programs.fish.enable = true;

  services.xserver = {
    enable = true;
    desktopManager.xfce.enable = true;
    displayManager = {
      lightdm.enable = true;
      autoLogin = {
        enable = true;
        user = "nixos";
      };
    };
  };

  services.chrony.enable = true;

  time.timeZone = "Europe/Berlin";
  boot.kernelPackages = pkgs.linuxPackages_latest;
  services.xserver.layout = "de";
  console.keyMap = "de";
}
