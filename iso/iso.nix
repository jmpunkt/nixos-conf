{config, pkgs, ...}:


{
  imports = [
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
    <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-graphical-kde.nix>
  ];

  time.timeZone = "Europe/Berlin";
  boot.kernelPackages = pkgs.linuxPackages_latest;
  services.xserver.layout = "de";
  i18n.consoleKeyMap = "de";
}
