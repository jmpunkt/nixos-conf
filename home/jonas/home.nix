{
  config,
  systemConfig,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ./fish
    ./profiles
  ];

  # Required by greetd to start wayfire with correct environment.
  programs.bash.enable = true;
  home.shell.enableBashIntegration = true;

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
  # Automatic garbage collection (user profiles)
  # TODO: compare with home-manager-auto-expire
  home.stateVersion = systemConfig.system.stateVersion;
}
