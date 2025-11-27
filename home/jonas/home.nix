{
  config,
  systemConfig,
  pkgs,
  lib,
  ...
}:
{
  imports = (import ../../modules/all-home-manager.nix) ++ [
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
  home.sessionVariables = {
    MOZ_ENABLE_WAYLAND = 1;
    NIXOS_OZONE_WL = 1;
  };
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "qemu" = {
        controlPersist = "10m";
        controlMaster = "auto";
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
  programs.nix-index.enable = true;
  services.lorri = {
    enable = true;
    nixPackage = systemConfig.nix.package;
  };
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };
  programs.mergiraf.enable = true;
  programs.difftastic.enable = true;
  programs.git = {
    enable = true;
    settings = {
      user.name = "Jonas Meurer";
      core = {
        whitespace = "trailing-space,space-before-tab";
      };
      rerere.enabled = "true";
      merge.conflictstyle = "diff3";
      pull.ff = "only";
    };
    lfs.enable = true;
  };
  # Automatic garbage collection (user profiles)
  # TODO: compare with home-manager-auto-expire
  nix.gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 14d";
    randomizedDelaySec = "5min";
  };
  home.stateVersion = systemConfig.system.stateVersion;
}
