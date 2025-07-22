{
  config,
  inputs,
  systemConfig,
  pkgs,
  ...
}:
let
  ifBluetooth = if systemConfig.hardware.bluetooth.enable then true else false;
in
{
  imports = [
    ./hyprland.nix
  ];

  home.packages = with pkgs; [
    #apps
    dolphin

    #utils
    brightnessctl
    shotman
  ];

  home.pointerCursor = {
    gtk.enable = true;
    name = "Breeze";
    package = pkgs.libsForQt5.breeze-qt5;
    size = 12;
  };

  services.gnome-keyring.enable = true;
  services.mpris-proxy.enable = ifBluetooth;
  services.blueman-applet.enable = ifBluetooth;

  gtk = {
    enable = true;
    iconTheme = {
      package = pkgs.libsForQt5.breeze-icons;
      name = "breeze";
    };
    theme = {
      package = pkgs.libsForQt5.breeze-gtk;
      name = "Breeze";
    };
    font = {
      name = "Sans";
      size = 11;
    };
  };
  # home.keyboard = {
  #     layout = "us";
  #     variant = "alt-intl";
  #     options = [ "caps:none" ];
  #   };
  qt = {
    enable = true;
    style = {
      name = "Breeze";
      package = pkgs.libsForQt5.breeze-qt5;
    };
  };

  home.file.${config.gtk.gtk2.configLocation}.force = true;
  xdg.configFile."gtk-3.0/settings.ini".force = true;
  # xdg.configFile."gtk-3.0/gtk.css".force = true;
  # xdg.configFile."gtk-3.0/bookmarks".force = true;
  xdg.configFile."gtk-4.0/settings.ini".force = true;
  # xdg.configFile."gtk-4.0/gtk.css".force = true;
  xdg.configFile."hypr/hyprland.conf".force = true;

  services.wlsunset = {
    enable = true;
    temperature = {
      day = 4500;
      night = 2800;
    };
    latitude = toString systemConfig.location.latitude;
    longitude = toString systemConfig.location.longitude;
    systemdTarget = "hyprland-session.target";
  };

  services.network-manager-applet.enable = true;

  systemd.user = {
    services.polkit-gnome-authentication-agent-1 = {
      Unit = {
        Description = "polkit-gnome-authentication-agent-1";
        Wants = [ "graphical-session.target" ];
        After = [ "graphical-session.target" ];
      };
      Service = {
        Type = "simple";
        ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
        Restart = "on-failure";
        RestartSec = 1;
        TimeoutStopSec = 10;
      };
      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };

  services.swayidle = {
    enable = true;
    systemdTarget = "hyprland-session.target";
    events = [
      {
        event = "after-resume";
        command = "hyprctl dispatch dpms on";
      }
      {
        event = "before-sleep";
        command = "${pkgs.swaylock}/bin/swaylock -f -c 000000";
      }
    ];
    timeouts = [
      {
        timeout = 300;
        command = "${pkgs.swaylock}/bin/swaylock -f -c 000000";
      }
      {
        timeout = 600;
        command = "hyprctl dispatch dpms off";
      }
      {
        timeout = 900;
        command = "systemctl suspend";
      }
    ];
  };
}
