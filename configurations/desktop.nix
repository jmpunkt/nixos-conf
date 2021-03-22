{ config, pkgs, ... }:

{
  imports =
    [ ./base.nix ./locale.nix ./shell.nix ./tmux.nix ./fish ./yubico.nix ];

  boot = {
    # Use newer kernel for desktop based machines
    kernelPackages = pkgs.linuxPackages_latest;
    supportedFilesystems =
      [ "btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs" ];
  };

  environment.systemPackages = with pkgs; [
    alacritty
    cryptsetup
    gnupg
    feh
    thunderbird
    firefox
    audacious

    discord
    tdesktop

    ntfsprogs
    dosfstools
    xfsprogs.bin
    jfsutils
    f2fs-tools
    ntfs3g

    tokei
    fzf
    git
    gitAndTools.delta
    hyperfine
    direnv
    dbeaver

    hunspell
    hunspellDicts.en-us-large
    jmpunkt.hunspellDicts.de-de

    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
  ];

  fonts = {
    fonts = with pkgs; [
      corefonts
      ibm-plex
      jetbrains-mono

      # Emacs Icons
      emacs-all-the-icons-fonts
      symbola
    ];

    fontconfig.defaultFonts.monospace = [ "Jetbrains Mono" ];
  };

  location = {
    latitude = 50.11;
    longitude = 8.682;
  };

  sound.enable = true;

  programs.blender = {
    enable = true;
    pythonPackages = pkgs.python3.withPackages
      (ps: with ps; [ certifi numpy ]);
  };

  hardware = {
    sane.enable = true;
    opengl.enable = true;
    pulseaudio.enable = true;
  };

  networking.networkmanager.enable = true;

  services = {
    redshift = {
      enable = true;
      brightness = {
        day = "1";
        night = "0.8";
      };
      temperature = {
        day = 6000;
        night = 3500;
      };
    };
    gamemode = {
      enable = false;
      ini = ''
        [general]
        reaper_freq=5
        desiredgov=performance
        softrealtime=off
        renice=0
        ioprio=0
        inhibit_screensaver=1
      '';
    };
    printing = {
      enable = true;
      drivers = with pkgs; [ gutenprint hplip ];
    };
    xserver = {
      enable = true;
      libinput = {
        enable = true;
        accelProfile = "flat";
      };
    };
  };
}
