{
  inputs,
  config,
  pkgs,
  lib,
  ...
}: let
  clipmon = pkgs.rustPlatform.buildRustPackage rec {
    pname = "clipmon";
    version = "11.0.2";

    src = pkgs.fetchFromSourcehut {
      owner = "~whynothugo/";
      repo = "clipmon ";
      rev = "2e338fdc2841c3b2de9271d90fcceceda9e45d29";
      sha256 = "sha256-kcUJVB1jP2qZ1YgJDEBsyn5AgwhRxQmzOrk0gKj1MeM=";
    };

    cargoSha256 = "17ldqr3asrdcsh4l29m3b5r37r5d0b3npq1lrgjmxb6vlx6a36q1";
  };
in {
  imports = [inputs.anyrun.homeManagerModules.default];

  services.greetd = let
    cmd = "Hyperland";
  in {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd \"${cmd}\"";
        user = "greeter";
      };
      initial_session = {
        command = "${cmd}";
        user = "jonas";
      };
    };
  };

  programs.anyrun = {
    enable = true;
    config = {
      plugins = with inputs.anyrun.packages.${pkgs.system}; [
        applications
        randr
        rink
      ];
      width = {fraction = 0.3;};
      position = "top";
      verticalOffset = {absolute = 0;};
      hideIcons = false;
      ignoreExclusiveZones = false;
      layer = "overlay";
      hidePluginInfo = false;
      closeOnClick = false;
      showResultsImmediately = false;
      maxEntries = null;
    };
  };

  environment.systemPackages = with pkgs; [
    dolphin

    #other
    hyperland
    swaylock
    swayidle
    wlsunset
    clipmon
    swaynotificationcenter
    nwg-displays
    nm-applet
    dolphin
  ];

  wayland.windowManager.hyprland.settings = {
    "$mainMod" = "SUPER";

    bind = [
      "$mainmod, L, exec, swaylock -f -c 000000"
      "$mainMod, E, exec, dolphin"

      "$mainMod , Q, killactive,"
      "$mainMod , S, togglesplit, # dwindle"
      "$mainMod , G, togglegroup,"
      "$mainMod ALT, SPACE, exec, anyrun"
      "$mainMod ALT, F9,  pseudo, # dwindle"
      "$mainMod ALT, F10, togglefloating,"
      "$mainMod ALT, F11, fullscreen, 0"

      "$mainMod CTRL ALT, Delete, exec, kill"
      "$mainMod CTRL SHIFT, ESC, exec, emacs -f proced"

      # window
      "$mainMod SHIFT, left, movewindow, l"
      "$mainMod SHIFT, right, movewindow, r"
      "$mainMod SHIFT, up, movewindow, u"
      "$mainMod SHIFT, down, movewindow, d"

      # move window
      "$mainMod ALT, left,moveactive,-50 0"
      "$mainMod ALT, down,moveactive, 0 50"
      "$mainMod ALT, up,moveactive, 0 -50"
      "$mainMod ALT, right,moveactive, 50 0"

      # workspace
      "$mainMod SHIFT, S,   movetoworkspace, special"
      "$mainMod SHIFT, F1,  movetoworkspace, special:1"
      "$mainMod SHIFT, F2,  movetoworkspace, special:2"
      "$mainMod SHIFT, F3,  movetoworkspace, special:3"
      "$mainMod SHIFT, F4,  movetoworkspace, special:4"
      "$mainMod SHIFT, F5,  movetoworkspace, special:5"
      "$mainMod SHIFT, F6,  movetoworkspace, special:6"

      "$mainMod, escape, execr, hyprctl dispatch togglespecialworkspace $specialWorkspaceId"
      "$mainMod, F1,  togglespecialworkspace, 1"
      "$mainMod, F2,  togglespecialworkspace, 2"
      "$mainMod, F3,  togglespecialworkspace, 3"
      "$mainMod, F4,  togglespecialworkspace, 4"
      "$mainMod, F5,  togglespecialworkspace, 5"
      "$mainMod, F6,  togglespecialworkspace, 6"
    ];

    bindle = [
      ", XF86AudioRaiseVolume,    exec, pactl set-sink-volume @DEFAULT_SINK@ +1%"
      ", XF86AudioLowerVolume,    exec, pactl set-sink-volume @DEFAULT_SINK@ -1%"
      ", XF86MonBrightnessUp,     exec, brightnessctl set 5%+ -q"
      ", XF86MonBrightnessDown,   exec, brightnessctl set 5%- -q"
      ", XF86KbdBrightnessUp,     exec, bash ~/.config/eww/scripts/brightness kbd up"
      ", XF86KbdBrightnessDown,   exec, bash ~/.config/eww/scripts/brightness kbd down"
    ];

    bindl = [
      ", XF86AudioStop,           exec, playerctl stop"
      ", XF86AudioPause,          exec, playerctl pause"
      ", XF86AudioPrev,           exec, playerctl previous"
      ", XF86AudioNext,           exec, playerctl next"
      ", XF86AudioPlay,           exec, playerctl play-pause"
    ];

    env = [
      "QT_WAYLAND_DISABLE_WINDOWDECORATION,1"
    ];

    exec_always = [
      "nm-applet --indicator"
    ];

    exec-once = [
      # "hyprctl setcursor ${pointer.name} ${toString pointer.size}"
      "swaylock"
      "wlsunset -l ${toString config.location.latitude} -L ${toString config.location.longitude} -t 2800 -T 4500"
      ''
        swayidle -w timeout 300 'swaylock -f -c 000000' \
                    timeout 600 'hyprctl dispatch dpms off' \
                    resume 'hyprctl dispatch dpms on' \
                    timeout 900 'systemctl suspend' \
                    before-sleep 'swaylock -f -c 000000' &
      ''
      "clipmon"
    ];

    general = {
      gaps_in = 5;
      gaps_out = 5;
      border_size = 1;
      "col.active_border" = "rgba(88888888)";
      "col.inactive_border" = "rgba(00000088)";

      allow_tearing = true;
      resize_on_border = true;
      layout = "dwindle";
    };

    decoration = {
      rounding = 16;
      blur = {
        enabled = true;
        brightness = 1.0;
        contrast = 1.0;
        noise = 0.02;

        passes = 3;
        size = 10;
      };

      drop_shadow = true;
      shadow_ignore_window = true;
      shadow_offset = "0 2";
      shadow_range = 20;
      shadow_render_power = 3;
      "col.shadow" = "rgba(00000055)";
    };

    animations = {
      enabled = true;
      animation = [
        "border, 1, 2, default"
        "fade, 1, 4, default"
        "windows, 1, 3, default, popin 80%"
        "workspaces, 1, 2, default, slide"
      ];
    };

    group = {
      groupbar = {
        font_size = 16;
        gradients = false;
      };
    };

    input = {
      kb_layout = config.console.keyMap;
      accel_profile = "flat";
      touchpad.scroll_factor = 0.1;
    };

    dwindle = {
      pseudotile = true;
      preserve_split = true;
    };

    misc = {
      disable_autoreload = true;
      force_default_wallpaper = 0;
      animate_mouse_windowdragging = false;
      vrr = 1;
      no_direct_scanout = false;
    };

    gestures = {
      workspace_swipe = true;
      workspace_swipe_forever = true;
    };

    xwayland.force_zero_scaling = true;
  };

  programs.waybar = {
    enable = true;
    settings = {
      "height" = 40;
      "position" = "top";
      "spacing" = 4;
      "modules-left" = ["hyprland/workspaces"];
      "modules-center" = ["hyprland/window"];
      "modules-right" = [
        "tray"
        "pulseaudio"
        "network"
        "backlight"
        "battery"
        "custom/monitor"
        "custom/notification"
        "clock"
      ];
      "hyprland/window" = {
        "format" = "{}";
      };
      "hyprland/workspaces" = {
        "format" = "{icon}";
        "on-click" = "activate";
        "persistent_workspaces" = {
          "1" = [];
          "2" = [];
          "3" = [];
          "4" = [];
          "5" = [];
          "6" = [];
        };
      };
      "custom/notification" = {
        "tooltip" = false;
        "format" = "{icon}";
        "format-icons" = {
          "notification" = "<span foreground='red'><sup></sup></span>";
          "none" = "";
          "dnd-notification" = "<span foreground='red'><sup></sup></span>";
          "dnd-none" = "";
          "inhibited-notification" = "<span foreground='red'><sup></sup></span>";
          "inhibited-none" = "";
          "dnd-inhibited-notification" = "<span foreground='red'><sup></sup></span>";
          "dnd-inhibited-none" = "";
        };
        "return-type" = "json";
        "exec-if" = "which swaync-client";
        "exec" = "swaync-client -swb";
        "on-click" = "swaync-client -t -sw";
        "on-click-right" = "swaync-client -d -sw";
        "escape" = true;
      };
      "tray" = {
        "spacing" = 10;
      };
      "clock" = {
        "tooltip-format" = "<big>{=%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
        "format-alt" = "{=%Y-%m-%d}";
      };
      "backlight" = {
        "format" = "{percent}% {icon}";
        "format-icons" = [
          ""
          ""
          ""
          ""
          ""
          ""
          ""
          ""
          ""
        ];
      };
      "battery" = {
        "states" = {
          # "good"= 95;
          "warning" = 30;
          "critical" = 15;
        };
        "format" = "{capacity}% {icon}";
        "format-full" = "{capacity}% {icon}";
        "format-charging" = "{capacity}% ";
        "format-plugged" = "{capacity}% ";
        "format-alt" = "{time} {icon}";
        "format-icons" = ["" "" "" "" ""];
      };
      "custom/monitor" = {
        on-click = "nwg-displays";
        exec-on-event = true;
        "format" = "🖵";
      };
      "pulseaudio" = {
        "format" = "{volume}% {icon} {format_source}";
        "format-bluetooth" = "{volume}% {icon} {format_source}";
        "format-bluetooth-muted" = " {icon} {format_source}";
        "format-muted" = " {format_source}";
        "format-source" = "{volume}% ";
        "format-source-muted" = "";
        "format-icons" = {
          "headphone" = "";
          "hands-free" = "";
          "headset" = "";
          "phone" = "";
          "portable" = "";
          "car" = "";
          "default" = ["" "" ""];
        };
        "on-click" = "pavucontrol";
      };
    };
  };

  # material-symbols
  # wired-notify
}
