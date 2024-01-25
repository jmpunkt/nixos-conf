{
  description = "A basic shell flake";

  inputs = {
    utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    utils,
  }: let
    mkVm = pkgs: system:
      (
        nixpkgs.lib.nixosSystem
        {
          inherit system;
          modules = [
            {
              nixpkgs.overlays = [
                self.overlays.default
              ];
            }
            "${nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
            self.nixosModules.default
            (
              {
                config,
                lib,
                ...
              }:
                with lib; {
                  options = {
                    virtualisation.qemu.forwardTCPPorts = mkOption {
                      type = types.listOf types.port;
                      description = mdDoc ''
                        Forwards these ports to the host system and allowing these ports in the firewall
                      '';
                    };
                  };

                  config =
                    mkIf (config.virtualisation.qemu.forwardTCPPorts != null)
                    (let
                      portsMapped = map (port: "hostfwd=tcp::${toString port}-:${toString port}") config.virtualisation.qemu.forwardTCPPorts;
                      configString = builtins.concatStringsSep "," portsMapped;
                    in {
                      networking.firewall.allowedTCPPorts = config.virtualisation.qemu.forwardTCPPorts;

                      virtualisation.qemu.networkingOptions = [
                        # We need to re-define our usermode network driver
                        # since we are overriding the default value.
                        "-net nic,netdev=user.1,model=virtio"
                        # Than we can use qemu's hostfwd option to forward ports.
                        "-netdev user,id=user.1,${configString}"
                      ];
                    });
                }
            )
            (
              {config, ...}: {
                services.openssh = {
                  ports = [11111];
                  enable = true;
                  settings = {
                    PermitRootLogin = "yes";
                  };
                };

                # no password for root user
                users = {
                  mutableUsers = false;
                  users.root = {
                    password = "";
                    isSystemUser = true;
                  };
                };

                # german layout for QEMU UI
                console = {
                  font = "Lat2-Terminus16";
                  keyMap = "de";
                };

                virtualisation = {
                  memorySize = 8192;
                  useBootLoader = false;
                  cores = 8;
                  qemu = {
                    package = pkgs.qemu;
                    forwardTCPPorts = [11111];
                  };
                };
              }
            )
          ];
        }
      )
      .config
      .system
      .build
      .vm;
  in
    (utils.lib.eachDefaultSystem
      (
        system: let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [self.overlays.default];
          };
        in {
          legacyPackages = pkgs;
          packages = {
            default = self.packages.${system}.vm;
            vm = mkVm pkgs system;
          };
          apps = {
            default = self.apps.${system}.vm;
            vm =
              utils.lib.mkApp
              {
                drv = self.packages.${system}.vm;
                exePath = "/bin/run-nixos-vm";
              };
          };
        }
      ))
    // {
      nixosModules.default = {config, ...}: {
        # put your configuration here, run with `nix run`
      };
      overlays.default = final: prev: {
        # put your overlay here
      };
    };
}
