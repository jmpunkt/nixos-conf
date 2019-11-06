{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    srm
    curl
    unzip
    tmux
    htop
    telnet
    fd
    exa

    git
    ripgrep
    direnv

    sdparm
    hdparm
    smartmontools
    pciutils
    usbutils
  ];

  hardware.cpu = {
    intel.updateMicrocode = true;
    amd.updateMicrocode = true;
  };

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
