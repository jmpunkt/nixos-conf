{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    srm
    wget
    git
    unzip
    curl
    tmux
    htop
    telnet
    ripgrep
    fd
    exa
    nox
    ntfs3g
    gcc
    gnumake
    home-manager
    direnv
    tokei
    fzf
    hyperfine
  ];

  programs.mtr.enable = true;
  programs.fish.enable = true;

  services.fstrim.enable = true;
  services.ntp.enable = true;
}
