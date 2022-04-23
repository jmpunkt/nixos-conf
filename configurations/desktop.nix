{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [./base.nix ./locale.nix ./shell.nix ./tmux.nix ./fish ./yubico.nix];
  boot = {
    # Use zen kernel for desktop based machines
    kernelPackages = pkgs.linuxKernel.packages.linux_zen;
    supportedFilesystems = ["btrfs" "reiserfs" "vfat" "f2fs" "xfs" "ntfs" "cifs"];
  };
  environment.systemPackages = with pkgs; [ntfsprogs dosfstools xfsprogs.bin jfsutils f2fs-tools ntfs3g chromium firefox-bin epiphany nyxt];
  fonts = {
    fonts = with pkgs; [
      corefonts
      ibm-plex
      jetbrains-mono
      # Emacs Icons
      emacs-all-the-icons-fonts
      symbola
    ];
    fontconfig.defaultFonts.monospace = ["Jetbrains Mono"];
  };
  location = {
    latitude = 50.11;
    longitude = 8.682;
  };
  sound.enable = lib.mkForce false;
  # programs.blender = {
  #   enable = true;
  #   pythonPackages = pkgs.python3.withPackages ( ps: with ps; [ certifi numpy ] );
  # };
  };
  hardware = {
    sane.enable = true;
    opengl.enable = true;
    pulseaudio.enable = lib.mkForce false;
  };
  networking.networkmanager.enable = true;
  systemd.services.NetworkManager-wait-online.enable = false;
  # pipewire (optional)
  security.rtkit.enable = true;
  services = {
    pipewire = {
      enable = true;
      pulse = {enable = true;};
      alsa = {
        enable = true;
        support32Bit = true;
      };
    };
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
      drivers = with pkgs; [gutenprint hplip];
    };
    xserver = {
      enable = true;
      libinput = {
        enable = true;
        touchpad.accelProfile = "flat";
      };
    };
  };
}
