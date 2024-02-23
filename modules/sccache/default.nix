{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.programs.sccache;
in {
  options = {
    services.sccache = {
      enable = mkEnableOption (lib.mdDoc "Sccache");
      package = lib.mkOption {
        type = lib.types.package;
        default = pkgs.sccache.overrideAttrs (old: {
          buildFeatures = ["dist-server"];
        });
        description = lib.mdDoc ''
          Package containing the sccache binary. MUST be build with the feature flag `dist-server`.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.services."sccache-scheduler" = let
      schedulerConfigToml = (pkgs.formats.toml {}).generate "scheduler" {
        public_addr = "127.0.0.1:10600";

        client_auth = {
          type = "DANGEROUSLY_INSECURE";
        };

        server_auth = {
          type = "DANGEROUSLY_INSECURE";
        };
      };
    in {
      enable = true;
      description = "Sccache";
      serviceConfig = {
        Type = "forking";
        StateDirectory = "sccache";
        ExecStart = "${cfg.package}/bin/sccache-dist scheduler --config ${schedulerConfigToml}";
      };
      wantedBy = ["multi-user.target"];
    };

    systemd.services."sccache-builder" = let
      builderConfigToml = (pkgs.formats.toml {}).generate "builder" {
        cache_dir = "/tmp/toolchains";
        public_addr = "127.0.0.1:10501";
        scheduler_url = "http://127.0.0.1:10600";

        builder = {
          type = "overlay";
          build_dir = "/tmp/sccache-build";
          bwrap_path = "${pkgs.bubblewrap}/bin/bwrap";
        };

        scheduler_auth = {
          type = "DANGEROUSLY_INSECURE";
        };
      };
    in {
      enable = true;
      description = "Sccache";
      serviceConfig = {
        StateDirectory = "sccache";
        ExecStart = "${cfg.package}/bin/sccache-dist server --config ${builderConfigToml}";
      };
      wantedBy = ["multi-user.target"];
    };
  };
}
