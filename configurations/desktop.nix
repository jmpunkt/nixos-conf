{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    alacritty
    home-manager
    tokei
    cryptsetup
    direnv
    gnupg
    feh
    thunderbird
    firefox
    dbeaver

    keepassxc

    texlive.combined.scheme-medium
    rustup

    discord
    steam

    tdesktop
    pencil
  ];

  services.printing.enable = true;
  services.xserver.enable = true;
  services.xserver.layout = "de";

  fonts.fonts = with pkgs; [
    corefonts
    dejavu_fonts
    ibm-plex
    fira
    fira-code
    fira-mono
    noto-fonts
    noto-fonts-extra
    noto-fonts-emoji
    roboto
    roboto-mono
    roboto-slab
  ];

  services.redshift = {
    enable = true;
    latitude = "50.110";
    longitude = "8.682";
  };
}
