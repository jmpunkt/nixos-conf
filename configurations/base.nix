{ config, pkgs, options, ... }:

{
  imports = import (../modules/all-nixos.nix);

  nix.nixPath = options.nix.nixPath.default
    ++ [ "nixpkgs-overlays=/etc/nixos/nixos-conf/overlays" ];

  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  nixpkgs.overlays =
    [ (import ../overlays/00-patch.nix) (import ../overlays/10-pkgs.nix) ];

  environment.systemPackages = with pkgs; [
    srm
    curl
    unzip
    htop
    telnet
    fd
    exa
    ripgrep

    sdparm
    hdparm
    smartmontools
    pciutils
    usbutils
  ];

  networking.firewall.enable = true;

  programs.fish.enable = true;

  services = {
    fstrim.enable = true;
    chrony.enable = true;
  };
}
