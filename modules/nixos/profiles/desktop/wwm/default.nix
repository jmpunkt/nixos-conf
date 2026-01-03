{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.profiles.desktop.wwm;
  wpctlBin = "${lib.getBin config.services.pipewire.wireplumber.package}/bin/wpctl";

  layoutOptions = lib.types.submodule {
    options = {
      default = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Indicate whether this layout is the default one. There can only be a single layout with default set to true.";
      };
      layout = lib.mkOption {
        type = lib.types.str;
        example = "us";
        description = "";
      };
      options = lib.mkOption {
        type = lib.types.str;
        default = "";
        example = "ctrl:nocaps";
        description = "";
      };
      variant = lib.mkOption {
        type = lib.types.str;
        default = "";
        example = "intl";
        description = "";
      };
    };
  };
in

{
  imports = [
    ./wayfire.nix
    ./hyprland.nix
  ];

  options.profiles.desktop.wwm = {
    enable = lib.mkEnableOption "Wayland Windows Manager support";
    loginSession = lib.mkOption {
      type = lib.types.path;
      description = "Command used to start the default login session. Specifically set by the window manager configuration.";
    };
    windowManager = lib.mkOption {
      type = lib.types.enum [
        "hyprland"
        "wayfire"
      ];
      description = "Windows manager used.";
    };
    idleManagement = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = !config.profiles.desktop.virtual;
        description = "Use idle timers and enable auto-locking and auto-suspense.";
      };
    };
    brightness = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = !config.profiles.desktop.virtual;
        description = "Use idle timers and enable auto-locking and auto-suspense.";
      };
      up = lib.mkOption {
        type = lib.types.str;
        default = "${lib.getExe pkgs.brightnessctl} set 5%+ -q";
        description = "Windows manager used.";
      };
      down = lib.mkOption {
        type = lib.types.str;
        default = "${lib.getExe pkgs.brightnessctl} set 5%- -q";
        description = "Windows manager used.";
      };
    };
    audio = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = !config.profiles.desktop.virtual;
        description = "Use idle timers and enable auto-locking and auto-suspense.";
      };
      volumeUp = lib.mkOption {
        type = lib.types.str;
        default = "${wpctlBin} set-volume @DEFAULT_AUDIO_SINK@ 5%+";
        description = "Increase volume on the default audio source.";
      };
      volumeDown = lib.mkOption {
        type = lib.types.str;
        default = "${wpctlBin} set-volume @DEFAULT_AUDIO_SINK@ 5%-";
        description = "Decrease volume on the default audio source.";
      };
      microphoneToggle = lib.mkOption {
        type = lib.types.str;
        default = "${wpctlBin} set-mute @DEFAULT_AUDIO_SOURCE@ toggle";
        description = "Toggle mute on default audio source.";
      };
      volumeToggle = lib.mkOption {
        type = lib.types.str;
        default = "${wpctlBin} set-mute @DEFAULT_AUDIO_SINK@ toggle";
        description = "Toggle mute on default audio sink.";
      };
    };
    finder = lib.mkOption {
      type = lib.types.path;
      default = lib.getExe pkgs.fuzzel;
      description = "Finder tool.";
    };
    fileExplorer = lib.mkOption {
      type = lib.types.path;
      default = lib.getExe pkgs.xfce.thunar;
      description = "Default command to open file explorer.";
    };
    keyboardLayouts = lib.mkOption {
      type = lib.types.listOf layoutOptions;
      description = "List of available keyboard XKB layouts.";
    };
  };

  # TODO: Set console.keyMap with default
  # TODO: Set in hyprland
  # TODO: Set in wayfire
  # assert builtins.any (layout: layout.default) cfg.keyboardLayouts;
  # assert (builtins.length (builtins.filter (layout: layout.default) cfg.keyboardLayouts)) == 1;

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      profiles.desktop.enable = true;

      security.polkit.enable = true;
      security.soteria.enable = true;
      services.gnome.gnome-keyring.enable = true;
      services.upower.enable = true;
      services.blueman.enable = config.hardware.bluetooth.enable or false;
      programs.thunar.enable = true;
      services.pipewire.wireplumber.enable = true;

      # Required for stylix
      programs.dconf.enable = true;

      services.greetd = {
        enable = true;
        settings = rec {
          initial_session = {
            user = config.services.displayManager.autoLogin.user;
            command = config.profiles.desktop.wwm.loginSession;
          };
          default_session = initial_session;
        };
      };
    })
    (lib.mkIf (cfg.enable && cfg.idleManagement.enable) {
      security.pam.services.swaylock = { };
    })
  ];
}
