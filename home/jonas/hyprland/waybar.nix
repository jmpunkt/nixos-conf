{
  config,
  pkgs,
  ...
}: let
  scripts = pkgs.callPackage ./scripts.nix {};
in {
  programs.waybar = {
    enable = true;
    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        spacing = 4;
        modules-left = ["hyprland/workspaces"];
        modules-right = [
          "tray"
          "pulseaudio"
          "backlight"
          "battery"
          "custom/monitor"
          "custom/notification"
          "custom/keyboard"
          "clock"
        ];
        "hyprland/workspaces" = {
          format = "{icon}";
          on-click = "activate";
          persistent-workspaces = {
            "1" = [];
            "2" = [];
            "3" = [];
            "4" = [];
            "5" = [];
            "6" = [];
          };
        };
        "custom/keyboard" = {
          tooltip = false;
          escape = true;
          format = "{icon} {}";
          icon = "⌨️";
          exex = "${scripts.keyboard-get}";
          interval = 5;
        };
        "custom/notification" = {
          tooltip = false;
          escape = true;
          format = "{icon}";
          format-icons = {
            notification = "<span foreground='red'><sup></sup></span>";
            none = "";
            dnd-notification = "<span foreground='red'><sup></sup></span>";
            dnd-none = "";
            inhibited-notification = "<span foreground='red'><sup></sup></span>";
            inhibited-none = "";
            dnd-inhibited-notification = "<span foreground='red'><sup></sup></span>";
            dnd-inhibited-none = "";
          };
          return-type = "json";
          exec = "${pkgs.swaynotificationcenter}/bin/swaync-client -swb";
          on-click = "${pkgs.swaynotificationcenter}/bin/swaync-client -t -sw";
          on-click-right = "${pkgs.swaynotificationcenter}/bin/swaync-client -d -sw";
        };
        tray = {
          spacing = 10;
        };
        clock = {
          format = "{:%H:%M}";
        };
        backlight = {
          tooltip = false;
          format = "{icon}";
          format-alt = "{percent}% {icon}";
          format-icons = [
            ""
          ];
        };
        battery = {
          states = {
            good = 60;
            warning = 30;
            critical = 15;
          };
          format = "{icon}";
          format-full = "{icon}";
          format-charging = "";
          format-charging-alt = "{capacity}% ";
          format-plugged = "";
          format-alt = "{time} {capacity}% {icon}";
          format-icons = ["" "" "" "" ""];
        };
        "custom/monitor" = {
          tooltip = false;
          on-click = "${pkgs.nwg-displays.override {hyprlandSupport = true;}}/bin/nwg-displays";
          format = "";
        };
        pulseaudio = {
          format = "{icon} {format_source}";
          format-bluetooth = "{icon} {format_source}";
          format-bluetooth-muted = " {icon} {format_source}";
          format-muted = " {format_source}";
          format-source = "";
          format-source-muted = "";
          format-icons = {
            headphone = "";
            hands-free = "";
            headset = "";
            phone = "";
            portable = "";
            car = "";
            default = ["" "" ""];
          };
          on-click = "${pkgs.pavucontrol}/bin/pavucontrol";
        };
      };
    };
    style = ''
      #custom-notification,
      #custom-monitor,
      #window,
      #clock,
      #battery,
      #pulseaudio,
      #workspaces,
      #tray,
      #backlight {
          background: #1e1e2e;
          padding: 0px 10px;
          margin: 3px 0px;
          border: 1px solid #181825;
          border-radius: 12px;
      }
      #workspaces button.focused {
          background-color: red;
      }
    '';
    systemd.enable = true;
    systemd.target = "hyprland-session.target";
  };
}
