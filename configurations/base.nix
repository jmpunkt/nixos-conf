{
  lib,
  config,
  pkgs,
  options,
  ...
}: {
  imports = [
    ./minimal.nix
  ];
  boot.binfmt.emulatedSystems = ["aarch64-linux"];
  environment.systemPackages = with pkgs; [srm curl unzip htop inetutils fd exa ripgrep git sdparm hdparm smartmontools pciutils usbutils];
  networking.firewall.enable = true;
  security.doas.enable = true;
  services.timesyncd.enable = true;
  programs = {
    fish.enable = true;
  };
}
