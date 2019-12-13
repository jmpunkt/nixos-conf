{config, pkgs, ...}:


let
  unstable = import (fetchTarball https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) { };
in
  {
    imports = [
      <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
      <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
    ];

    time.timeZone = "Europe/Berlin";
    boot.kernelPackages = unstable.pkgs.linuxPackages_5_3;
    services.xserver.layout = "de";
    i18n.consoleKeyMap = "de";
  }
