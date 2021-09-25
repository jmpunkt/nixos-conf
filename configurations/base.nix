{ lib, config, pkgs, options, ... }:

{
  imports = import (../modules/all-nixos.nix);

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    binaryCaches = [
      "https://nix-community.cachix.org"
    ];
    binaryCachePublicKeys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
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
    git

    sdparm
    hdparm
    smartmontools
    pciutils
    usbutils
  ];

  networking.firewall.enable = true;

  programs = {
    fish.enable = true;
    # Disable command-not-found since there is probably no nixos
    # channel
    command-not-found.enable = lib.mkForce false;
  };

  security.doas.enable = true;
  security.sudo.enable = lib.mkForce false;

  services.timesyncd.enable = true;
}
