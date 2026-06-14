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
  programs.opencode = {
    enable = true;
    package = pkgs.unstable.opencode;
    settings = {
      permission = {
        read = "allow";
        edit = "allow";
        bash = "ask";
        webfetch = "allow";
        skill = "allow";
        question = "allow";
      };
      provider = {
        ollama = {
          npm = "@ai-sdk/openai-compatible";
          name = "Ollama (local)";
          options = {
            baseURL = "http://localhost:11434/v1";
          };
          models = {
            "lfm2.5-thinking=latest" = {
              name = "LFM2 2.5 Thinking";
            };
          };
        };
      };
    };
  };
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    settings = {
      qemu = lib.hm.dag.entryBefore [ "*" ] {
        ControlMaster = "auto";
        ControlPersist = "10m";
        HostName = "127.0.0.1";
        Port = "11111";
        User = "root";
        StrictHostKeyChecking = "no";
        UserKnownHostsFile = "/dev/null";
      };

      "*" = {
        AddKeysToAgent = "yes";
        ForwardAgent = "no";
        Compression = "no";
        ControlMaster = "no";
        ControlPath = "~/.ssh/master-%r@%n:%p";
        ControlPersist = "no";
        ServerAliveCountMax = "3";
        ServerAliveInterval = "0";
        UserKnownHostsFile = "~/.ssh/known_hosts";
      };
    };
  };
  programs.nix-index.enable = true;
  # Automatic garbage collection (home manager generations)
  # NOTE: The actual GC of the Nix store is handled in the NixOS module.
  services.home-manager.autoExpire = {
    enable = true;
    frequency = "weekly";
    timestamp = "-14 days";
  };
  home.stateVersion = systemConfig.system.stateVersion;
}
