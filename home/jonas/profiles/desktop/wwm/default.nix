{
  systemConfig,
  lib,
  pkgs,
  config,
  inputs,
  ...
}:

let
  cfg = systemConfig.profiles.desktop.wwm;

  swayncClientBin = "${lib.getBin config.services.swaync.package}/bin/swaync-client";
  wlogoutBin = lib.getExe config.programs.wlogout.package;
in
{
  imports = [
    inputs.stylix.homeModules.default
    ./wayfire
    ./hyprland
  ];

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      stylix.enable = true;
      stylix.autoEnable = false;
      stylix.targets.gtk.enable = true;
      stylix.targets.qt.enable = true;
      stylix.targets.fish.enable = true;
      stylix.targets.font-packages.enable = true;
      stylix.targets.fontconfig.enable = true;

      stylix.targets.fuzzel.enable = true;

      gtk.gtk2.force = true;
      xdg.configFile."gtk-4.0/settings.ini".force = true;
      xdg.configFile."gtk-4.0/gtk.css".force = true;
      xdg.configFile."gtk-3.0/settings.ini".force = true;
      xdg.configFile."gtk-3.0/gtk.css".force = true;
      xdg.configFile."qt5ct/qt5ct.conf".force = true;

      stylix.fonts = {
        serif = {
          package = pkgs.noto-fonts;
          name = "Noto Serif";
        };
        sansSerif = {
          package = pkgs.noto-fonts;
          name = "Noto Sans";
        };
        monospace = {
          package = pkgs.fantasque-sans-mono;
          name = "Fantasque Sans Mono";
        };
        emoji = {
          package = pkgs.noto-fonts-color-emoji;
          name = "Noto Color Emoji";
        };

        sizes = {
          desktop = 12;
          applications = 12;
          terminal = 12;
          popups = 12;
        };
      };

      stylix.base16Scheme = {
        base00 = "#0d0e1c"; # bg-main
        base01 = "#292d48"; # bg-mode-line-inactive
        base02 = "#2b3045"; # bg-inactive
        base03 = "#4a4f69"; # bg-active
        base04 = "#484d67"; # bg-mode-line-active
        base05 = "#ffffff"; # fg-main
        base06 = "#ffffff"; # fg-main
        base07 = "#ffffff"; # fg-main
        base08 = "#ff5f59"; # red
        base09 = "#fec43f"; # yellow-warmer
        base0A = "#d0bc00"; # yellow
        base0B = "#44bc44"; # green
        base0C = "#00d3d0"; # cyan
        base0D = "#2fafff"; # blue
        base0E = "#feacd0"; # magenta
        base0F = "#db8b3f"; # rust
      };

      stylix.targets.swaync.enable = true;
      stylix.targets.waybar.enable = true;
      stylix.targets.ghostty.enable = true;

      stylix.cursor = {
        name = "Adwaita";
        package = pkgs.adwaita-icon-theme;
        size = 24;
      };

      home.packages = with pkgs; [
        wdisplays
        brightnessctl
        pavucontrol
        wlogout
      ];

      services.gpg-agent = {
        pinentry.package = pkgs.pinentry-gnome3;
      };
      # NOTE: Required for pinentry-gnome3 to work
      dbus.packages = with pkgs; [ gcr ];

      home.sessionVariables = {
        GTK_USE_PORTAL = "1";
      };

      services.swaync.enable = true;

      services.blueman-applet.enable = systemConfig.hardware.bluetooth.enable or false;
      programs.ghostty.enable = true;
      services.cliphist.enable = true;
      services.network-manager-applet.enable = true;
      services.poweralertd.enable = true;
      services.wlsunset = {
        enable = true;
        latitude = systemConfig.location.latitude;
        longitude = systemConfig.location.longitude;
        temperature = {
          night = 2500;
          day = 4500;
        };
      };

      systemd.user.services.waybar.Service.Environment = "PATH=${config.home.profileDirectory}/bin";
      programs.waybar = {
        enable = true;
        systemd.enable = true;
        style = ''
          * {
            font-family: "Symbols Nerd Font";
          }

          #idle_inhibitor,
          #battery,
          #pulseaudio,
          #custom-notification,
          #custom-logout {
            font-size: 24px;
          }

          #custom-logout,
          #custom-notification {
            padding: 0 10px;
            padding: 0 10px;
          }
        '';
        settings = {
          mainBar = {
            layer = "top";
            position = "top";
            height = 42;

            modules-center = [ ];

            modules-right = [
              "tray"
              "pulseaudio"
              "battery"
              "idle_inhibitor"
              "custom/notification"
              "clock"
              "custom/logout"
            ];

            tray = {
              icon-size = 24;
              spacing = 4;
            };
            idle_inhibitor = {
              format = "{icon}";
              format-icons = {
                activated = "";
                deactivated = "";
              };
            };
            clock = {
              interval = 1;
              format = "{:%F\n%H:%M:%S}";
            };
            "custom/logout" = {
              on-click = wlogoutBin;
              format = "<span></span>";
            };
            "custom/notification" = {
              tooltip = false;
              format = "{} <span>{icon}</span>";
              format-icons = {
                notification = "󱅫";
                none = "";
                dnd-notification = " ";
                dnd-none = "󰂛";
                inhibited-notification = " ";
                inhibited-none = "";
                dnd-inhibited-notification = " ";
                dnd-inhibited-none = " ";
              };
              return-type = "json";
              exec = "${swayncClientBin} -swb";
              on-click = "${swayncClientBin} -t -sw";
              on-click-right = "${swayncClientBin} -d -sw";
              escape = true;
            };
            battery = {
              states = {
                good = 60;
                warning = 30;
                critical = 15;
              };
              format = "{icon}";
              format-full = "{icon}";
              format-charging = "";
              format-charging-alt = "{capacity}% ";
              format-plugged = "";
              format-alt = "{time} {capacity}% {icon}";
              format-icons = [
                ""
                ""
                ""
                ""
                ""
              ];
            };
            pulseaudio = {
              format = "{icon} {format_source}";
              format-bluetooth = "{icon} {format_source}";
              format-bluetooth-muted = " {icon} {format_source}";
              format-muted = " {format_source}";
              format-source = "";
              format-source-muted = "";
              format-icons = {
                headphone = "";
                hands-free = "󱡏";
                headset = "󰋎";
                phone = "";
                portable = "";
                car = "";
                default = [
                  ""
                  ""
                  ""
                ];
              };
              on-click = "${pkgs.pavucontrol}/bin/pavucontrol";
            };
          };
        };
      };
    })
    (lib.mkIf (cfg.enable && systemConfig.profiles.desktop.wwm.idleManagement) {
      programs.swaylock.enable = true;
      stylix.targets.swaylock.enable = true;
      services.swayidle = {
        enable = true;
        timeouts = [
          {
            timeout = 300;
            command = "${lib.getExe config.programs.swaylock.package} -fF";
          }
          {
            timeout = 600;
            command = "${pkgs.wlopm}/bin/wlopm --off *";
          }
          {
            timeout = 900;
            command = "systemctl suspend";
          }
        ];
        events = [
          {
            event = "before-sleep";
            command = "${lib.getExe config.programs.swaylock.package} -fF";
          }
          {
            event = "after-resume";
            command = "${pkgs.wlopm}/bin/wlopm --on *";
          }
        ];
      };
    })
  ];
}
