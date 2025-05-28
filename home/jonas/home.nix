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
      ./tags.nix
      ./fish
    ];
  manual.manpages.enable = false;
  home.language = {
    base = "en_IE.UTF-8";
    monetary = "de_DE.utf8";
    telephone = "de_DE.utf8";
    address = "de_DE.utf8";
  };
  programs.ssh = {
    enable = true;
    controlMaster = "auto";
    controlPersist = "10m";
    matchBlocks = {
      "qemu" = {
        hostname = "127.0.0.1";
        port = 11111;
        user = "root";
        extraOptions = {
          StrictHostKeyChecking = "no";
          UserKnownHostsFile = "/dev/null";
        };
      };
    };
  };
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
      lfs.enable = true;
      extraConfig = {
        core = {
          whitespace = "trailing-space,space-before-tab";
        };
        rerere.enabled = "true";
        merge.conflictstyle = "zdiff3";
        pull.ff = "only";
      };
    };
  };
  # Automatic garbage collection (user profiles)
  # TODO: compare with home-manager-auto-expire
  nix.gc = {
    automatic = true;
    frequency = "daily";
    options = "--delete-older-than 14d";
    randomizedDelaySec = "5min";
  };
  home.stateVersion = "25.05";
}
