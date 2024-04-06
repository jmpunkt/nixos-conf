{
  config,
  systemConfig,
  pkgs,
  ...
}: let
  clipmon = pkgs.rustPlatform.buildRustPackage rec {
    pname = "clipmon";
    version = "11.0.2";

    src = pkgs.fetchFromSourcehut {
      owner = "~whynothugo";
      repo = "clipmon";
      rev = "2e338fdc2841c3b2de9271d90fcceceda9e45d29";
      sha256 = "sha256-bEMgJYz3e2xwMO084bmCT1oZImcmO3xH6rIsjvAxnTA=";
    };

    cargoSha256 = "sha256-N3sfVwHpBkFxRsvOSpwqhrb31018myNnTKiOWMEitFo=";
  };
  scripts = pkgs.callPackage ./scripts.nix {};
in {
  wayland.windowManager.hyprland = {
    enable = true;
    settings = let
      mainMod = "SUPER";
    in {
      bind = [
        "${mainMod}, L, exec, swaylock -f -c 000000"
        "${mainMod}, E, exec, dolphin"
        "${mainMod}, P, exec, shotman"
        "${mainMod}, R, exec, emacs"
        "${mainMod}, O, exec, ${scripts.keyboard-switch}"

        "${mainMod} , Q, killactive,"
        "${mainMod} , S, togglesplit, # dwindle"
        "${mainMod} , G, togglegroup,"
        "${mainMod} ALT, F9,  pseudo, # dwindle"
        "${mainMod} ALT, F10, togglefloating,"
        "${mainMod} ALT, F11, fullscreen, 0"

        "CTRL ALT, Delete, exec, kill"
        "CTRL SHIFT, ESC, exec, emacs -f proced"

        # window
        "${mainMod}, left, movefocus, l"
        "${mainMod}, right, movefocus, r"
        "${mainMod}, up, movefocus, u"
        "${mainMod}, down, movefocus, d"
        "${mainMod} SHIFT, left, movewindow, l"
        "${mainMod} SHIFT, right, movewindow, r"
        "${mainMod} SHIFT, up, movewindow, u"
        "${mainMod} SHIFT, down, movewindow, d"
        "${mainMod} CTRL, left, resizeactive, -20 0"
        "${mainMod} CTRL, right, resizeactive, 20 0"
        "${mainMod} CTRL, up, resizeactive, 0 -20"
        "${mainMod} CTRL, down, resizeactive, 0 20"

        # workspace
        "${mainMod}, 1,  workspace, 1"
        "${mainMod}, 2,  workspace, 2"
        "${mainMod}, 3,  workspace, 3"
        "${mainMod}, 4,  workspace, 4"
        "${mainMod}, 5,  workspace, 5"
        "${mainMod}, 6,  workspace, 6"

        "${mainMod} SHIFT, 1,  movetoworkspace, 1"
        "${mainMod} SHIFT, 2,  movetoworkspace, 2"
        "${mainMod} SHIFT, 3,  movetoworkspace, 3"
        "${mainMod} SHIFT, 4,  movetoworkspace, 4"
        "${mainMod} SHIFT, 5,  movetoworkspace, 5"
        "${mainMod} SHIFT, 6,  movetoworkspace, 6"

        # workspace special
        "${mainMod} SHIFT, S,   movetoworkspace, special"
        "${mainMod} SHIFT, F1,  movetoworkspace, special:1"
        "${mainMod} SHIFT, F2,  movetoworkspace, special:2"
        "${mainMod} SHIFT, F3,  movetoworkspace, special:3"
        "${mainMod} SHIFT, F4,  movetoworkspace, special:4"
        "${mainMod} SHIFT, F5,  movetoworkspace, special:5"
        "${mainMod} SHIFT, F6,  movetoworkspace, special:6"

        "${mainMod}, escape, execr, hyprctl dispatch togglespecialworkspace $specialWorkspaceId"
        "${mainMod}, F1,  togglespecialworkspace, 1"
        "${mainMod}, F2,  togglespecialworkspace, 2"
        "${mainMod}, F3,  togglespecialworkspace, 3"
        "${mainMod}, F4,  togglespecialworkspace, 4"
        "${mainMod}, F5,  togglespecialworkspace, 5"
        "${mainMod}, F6,  togglespecialworkspace, 6"
      ];

      bindle = [
        ", XF86AudioRaiseVolume,    exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"
        ", XF86AudioLowerVolume,    exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
        ", XF86AudioMute,           exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
        ", XF86AudioMicMute,        exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"
        ", XF86MonBrightnessUp,     exec, brightnessctl set 5%+ -q"
        ", XF86MonBrightnessDown,   exec, brightnessctl set 5%- -q"
        ", XF86KbdBrightnessUp,     exec, brightnessctl set 5%+ -q"
        ", XF86KbdBrightnessDown,   exec, brightnessctl set 5%- -q"
      ];

      env = [
        "QT_WAYLAND_DISABLE_WINDOWDECORATION,1"
        "WLR_DRM_NO_ATOMIC,1"
      ];

      exec-once = [
        "${clipmon}/bin/clipmon"
        "${pkgs.swaynotificationcenter}/bin/swaync"
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
        # kb_layout = systemConfig.console.keyMap;
        kb_layout = "de,us";
        kb_options = "caps:ctrl_modifier,";
        kb_variant = ",altgr-intl";
        accel_profile = "flat";
        touchpad.scroll_factor = 0.5;
        sensitivity = 0;
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

      windowrule = [
        "float, pavucontrol"
      ];

      windowrulev2 = [
        # "class:(Emacs),workspace:1"
        # "class:(Firefox),workspace:2"
        # "class:(Chatterino),workspace:6"
      ];

      xwayland.force_zero_scaling = true;
    };
  };

  # material-symbols
  # wired-notify
}
