{ pkgs, config, ... }:

with config;
{
  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
    };
    autoOptimiseStore = true;
    buildMachines = [
      { hostName = "localhost";
        systems = [ "x86_64-linux" "i686-linux" ];
        supportedFeatures = ["kvm" "nixos-test" "big-parallel" "benchmark"];
        maxJobs = 4;
      }
    ];
    trustedUsers = ["hydra" "hydra-evaluator" "hydra-queue-runner"];
  };

  networking.firewall.allowedTCPPorts = [ 80 ];

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_11;
    identMap = ''
        hydra-users hydra hydra
        hydra-users hydra-queue-runner hydra
        hydra-users hydra-www hydra
        hydra-users root postgres
        hydra-users postgres postgres
    '';
  };

  services.hydra = {
    enable = true;
    hydraURL = "http://localhost:3000";
    notificationSender = "hydra@localhost";
    buildMachinesFiles = [];
    useSubstitutes = true;
    extraConfig = ''
      store_uri=file:///var/lib/hydra/cache?secret-key=/etc/nix/${networking.hostName}/secret
    '';
  };

  networking.defaultMailServer = {
    directDelivery = true;
    hostName = "nixos";
    domain = "nixos.local";
  };

  services.nginx = {
    enable = true;

    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;

    virtualHosts."_" = {
      # forceSSL = true;
      # enableACME = true;
      locations."/".proxyPass = "http://localhost:3000";
    };
  };

  systemd.services.hydra-manual-setup = {
    description = "Create Admin User for Hydra";
    serviceConfig.Type = "oneshot";
    serviceConfig.RemainAfterExit = true;
    wantedBy = [ "multi-user.target" ];
    requires = [ "hydra-init.service" ];
    after = [ "hydra-init.service" ];
    environment = builtins.removeAttrs (config.systemd.services.hydra-init.environment) ["PATH"];

    script = ''
    if [ ! -e ~hydra/.setup-is-complete ]; then
      # create signing keys
      /run/current-system/sw/bin/install -d -m 551 /etc/nix/${networking.hostName}
      /run/current-system/sw/bin/nix-store --generate-binary-cache-key ${networking.hostName} /etc/nix/${networking.hostName}/secret /etc/nix/${networking.hostName}/public
      /run/current-system/sw/bin/chown -R hydra:hydra /etc/nix/${networking.hostName}
      /run/current-system/sw/bin/chmod 440 /etc/nix/${networking.hostName}/secret
      /run/current-system/sw/bin/chmod 444 /etc/nix/${networking.hostName}/public
      # create cache
      /run/current-system/sw/bin/install -d -m 755 /var/lib/hydra/cache
      /run/current-system/sw/bin/chown -R hydra-queue-runner:hydra /var/lib/hydra/cache
      # done
      touch ~hydra/.setup-is-complete
    fi
  '';
  };
}
