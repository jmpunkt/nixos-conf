{
  modulesPath,
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [
    ../../configurations/rpi2.nix
    ../../configurations/locale.nix
    ../../configurations/users/root.nix
  ];

  systemd.services."sensor-data-setup" = {
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      Type = "oneshot";
      WorkingDirectory = config.users.users.sensor-data.home;
      User = "sensor-data";
      ExecStart =
        let
          script = pkgs.writeScript "sensor-data-start" ''
            #!${pkgs.runtimeShell}
            if [ ! -f "mqtt.sqlite" ]; then
              ${pkgs.sqlite}/bin/sqlite3 mqtt.sqlite 'PRAGMA journal_mode=WAL;'
              chmod 760 mqtt.sqlite
            fi
          '';
        in
        "${script}";
    };
  };

  environment.systemPackages = [ pkgs.sqlite ];

  users.users.sensor-data = {
    isSystemUser = true;
    group = "sensor-data";
    home = "/var/lib/sensor-data";
    createHome = true;
  };
  users.groups.sensor-data = { };
  users.users.grafana.extraGroups = [ "sensor-data" ];
  users.users.telegraf.extraGroups = [ "sensor-data" ];

  services.telegraf = {
    enable = true;
    extraConfig = {
      inputs = {
        mqtt_consumer = {
          servers = [ "tcp://127.0.0.1:1883" ];
          topics = [
            "tasmota/tele/+/SENSOR"
          ];
          data_format = "json";
        };
      };
      outputs = {
        sql = {
          driver = "sqlite";
          data_source_name = "/var/lib/sensor-data/mqtt.sqlite";
        };
      };
    };
  };

  services.mosquitto = {
    enable = true;
    listeners = [
      {
        port = 1883;
        settings = {
          allow_anonymous = true;
        };
      }
    ];
  };

  networking = {
    firewall = {
      enable = true;
      allowedTCPPorts = [
        80
        443
        1883
      ];
    };
    hostName = "rpi2";
    interfaces.enp1s0 = {
      useDHCP = false;
      ipv4.addresses = [
        {
          address = "192.168.178.5";
          prefixLength = 24;
        }
      ];
    };
    defaultGateway.address = "192.168.178.1";
    nameservers = [ "192.168.178.1" ];
  };
}
