{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.profiles.desktop;
in
{
  imports = [
    ./kde.nix
    ./cosmic.nix
    ./xfce.nix
    ./wwm
  ];

  options.profiles.desktop = {
    enable = lib.mkEnableOption "Basic desktop support";
    virtual = lib.mkEnableOption "Virtual machine mode";
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      profiles.minimal.enable = true;
      boot = {
        kernelPackages = pkgs.linuxPackages_latest;
        supportedFilesystems = [
          "btrfs"
          "reiserfs"
          "vfat"
          "f2fs"
          "xfs"
          "ntfs"
          "cifs"
        ];
      };

      hardware.graphics.enable = true;
      hardware.bluetooth.enable = true;

      services.libinput = {
        enable = true;
        touchpad = {
          accelProfile = "flat";
          naturalScrolling = true;
          tapping = true;
        };
      };

      programs.nh = {
        enable = true;
        package = pkgs.unstable.nh;
      };

      system.rebuild.enableNg = true;

      # NOTE: Claims to have better performance.
      services.dbus.implementation = "broker";

      services.scx.enable = true;
      services.scx.scheduler = "scx_rusty";
      # Automatic garbage collection (system profiles)
      nix.gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 14d";
        randomizedDelaySec = "15min";
      };

      fonts = {
        # Render emoji font in Firefox correctly (https://discourse.nixosstag.fcio.net/t/firefox-doesnt-render-noto-color-emojis/51202/2)
        fontconfig.useEmbeddedBitmaps = true;
        packages = with pkgs; [
          dejavu_fonts
          freefont_ttf
          liberation_ttf
          noto-fonts
          noto-fonts-color-emoji
          # Monospace
          fantasque-sans-mono
          nerd-fonts.symbols-only
        ];
      };
      networking.networkmanager.enable = true;
      networking.wireless.enable = lib.mkForce false;
      systemd.services.NetworkManager-wait-online.enable = false;
      # Remove logs older than 3 days.
      services.journald.extraConfig = ''
        MaxFileSec=3day
      '';
    })

    (lib.mkIf (cfg.enable && !cfg.virtual) {
      nix.settings = {
        substituters = [
          "https://emacs-ci.cachix.org"
        ];
        trusted-public-keys = [
          "emacs-ci.cachix.org-1:B5FVOrxhXXrOL0S+tQ7USrhjMT5iOPH+QN9q0NItom4="
        ];
      };

      # scanner
      hardware = {
        sane.enable = true;
        sane.extraBackends = with pkgs; [ hplipWithPlugin ];
        sane.disabledDefaultBackends = [ "escl" ];
      };

      environment.systemPackages = with pkgs; [
        simple-scan
      ];

      # sound
      security.rtkit.enable = true;
      services = {
        pipewire = {
          enable = true;
          pulse = {
            enable = true;
          };
          alsa = {
            enable = true;
            support32Bit = true;
          };
        };
      };

      # printing
      services = {
        printing.enable = true;
        printing.drivers = with pkgs; [ hplip ];
        avahi = {
          enable = true;
          nssmdns4 = true;
          nssmdns6 = true;
        };
      };

      # dns
      networking.nameservers = [
        # Quad9
        "9.9.9.9"
        "149.112.112.112"
        "2620:fe::fe"
        "2620:fe::9"
      ];
      services.resolved = {
        enable = true;
        dnssec = "true";
        dnsovertls = "opportunistic";
        llmnr = "false";
        fallbackDns = [
          # Cloudflare
          "1.1.1.1"
          "1.0.0.1"
          "2606:4700:4700::1111"
          "2606:4700:4700::1001"
        ];
      };
      networking.networkmanager = {
        settings.connection = {
          "ipv6.addr-gen-mode" = "stable-privacy";
          "ipv6.ip6-privacy" = "2";
        };
      };
    })
  ];
}
