{
  config,
  systemConfig,
  pkgs,
  lib,
  ...
}: {
  imports =
    (import ../../modules/all-home-manager.nix)
    ++ [
      (
        if systemConfig.programs.hyprland.enable
        then import ./hyprland
        else {...}: {}
      )
    ];
  home.language = {
    base = "en_IE.UTF-8";
    monetary = "de_DE.utf8";
    telephone = "de_DE.utf8";
  };
  xdg.configFile = {
    "emacs/init.el".text = builtins.readFile ./emacs/init.el;
    # since emacs 27.1
    "emacs/early-init.el".text = builtins.readFile ./emacs/early-init.el;
  };
  home.file = {
    ".ssh/id_rsa.pub".text = builtins.readFile ./ssh/yubikey.pub;
    ".ssh/config".text = builtins.readFile ./ssh/config;
  };
  manual.manpages.enable = false;
  home.sessionVariables = {
    MOZ_ENABLE_WAYLAND = 1;
    NIXOS_OZONE_WL = 1;
  };
  services.syncthing = {
    enable = true;
    tray.enable = true;
  };
  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    git = {
      enable = true;
      userName = "Jonas Meurer";
      userEmail = "jmpunkt@outlook.com";
      lfs.enable = true;
      signing = {
        key = "4D78720A4358CC504F3EB45B26CDFB2E4DB6B136";
        signByDefault = true;
      };
      extraConfig = {
        core = {
          whitespace = "trailing-space,space-before-tab";
        };
        rerere.enabled = "true";
        merge.conflictstyle = "zdiff3";
        pull.ff = "only";
      };
    };
    emacs = {
      enable = true;
      package = pkgs.jmpunkt.emacs;
    };
  };
  # services.emacs = {
  #   enable = true;
  #   package = pkgs.jmpunkt.emacs;
  #   startWithUserSession = "graphical";
  #   client.enable = true;
  #   defaultEditor = true;
  # };
  home.stateVersion = "18.09";
}
