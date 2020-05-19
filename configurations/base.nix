{ config, pkgs, options, ... }:

{
  nix.nixPath = options.nix.nixPath.default
    ++ [ "nixpkgs-overlays=/etc/nixos/nixos-conf/overlays" ];

  environment.systemPackages = with pkgs; [
    srm
    curl
    unzip
    tmux
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

  programs = {
    mtr.enable = true;
    fish.enable = true;
  };

  services = {
    fstrim.enable = true;
    ntp.enable = true;
  };
}
