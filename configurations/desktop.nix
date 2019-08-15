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

    discord
    steam

    tdesktop
    pencil

    texlive.combined.scheme-medium
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

    # Emacs Icons
    # material-design-icons
    # weather-icons
    # font-awesome_5
    emacs-all-the-icons-fonts
    # file-icons
    # octicons
  ];

  services.redshift = {
    enable = true;
    latitude = "50.110";
    longitude = "8.682";
  };
}
