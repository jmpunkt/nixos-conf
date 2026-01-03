{
  systemConfig,
  lib,
  pkgs,
  config,
  ...
}:

let
  cfg = systemConfig.profiles.desktop.wwm;
in
{
  config = lib.mkIf (cfg.enable && cfg.windowManager == "wayfire") {
    stylix.targets.wayfire.enable = true;

    wayland.windowManager.wayfire = {
      enable = true;
      systemd.enable = true;
      systemd.variables = [
        "DISPLAY"
        "NIXOS_OZONE_WL"
        "WAYLAND_DISPLAY"
        "XCURSOR_SIZE"
        "XCURSOR_THEME"
        "XDG_CURRENT_DESKTOP"
        "XDG_SESSION_ID" # required for soteria
        "SSH_AUTH_SOCK"
      ];
      plugins = with pkgs.wayfirePlugins; [
        wayfire-plugins-extra
      ];
      settings = {
        input = {
          xkb_layout = "de,us(intl)";
          xkb_options = "ctrl:nocaps";
        };
        core = {
          plugins = builtins.concatStringsSep " " [
            "command"
            "decoration"
            "foreign-toplevel"
            "gtk-shell"
            "idle"
            "resize"
            "place"
            "wm-actions"
            "ipc"
            "ipc-rules"
            "move"
            "switcher"
            "session-lock"
            "xdg-activation"
          ];
        };
        # TODO: audio keys
        command = {
          binding_launcher = "<super>";
          command_launcher = cfg.finder;

          binding_files = "<super> KEY_E";
          command_files = cfg.fileExplorer;
        }
        // (lib.optionalAttrs cfg.idleManagement.enable {
          binding_lock = "<super> KEY_L";
          command_lock = lib.getExe config.programs.swaylock.package;
        })
        // (lib.optionalAttrs cfg.brightness.enable {
          binding_brightness_down = "KEY_BRIGHTNESSDOWN";
          command_brightness_down = cfg.brightness.down;

          binding_brightness_up = "KEY_BRIGHTNESSUP";
          command_brightness_up = cfg.brightness.up;
        });
        place = {
          mode = "center";
        };
        switcher = {
          next_view = "<alt> KEY_TAB";
          prev_view = "<alt> <shift> KEY_TAB";
        };
        wm-actions = {
          toggle_maximize = "<super> KEY_F";
        };
      };
    };

    systemd.user.services.waybar.Service.Environment = "PATH=${config.home.profileDirectory}/bin";
    programs.waybar = {
      enable = true;
      systemd.enable = true;
      settings = {
        mainBar = {
          modules-left = [
            "wlr/taskbar"
          ];

          "wlr/taskbar" = {
            format = "{icon} {name}";
            on-click = "minimize-raise";
            icon-size = 24;
          };
        };
      };
    };
  };
}
