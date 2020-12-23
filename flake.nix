{
  description = "My configuration as a flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
    hardware.url = "github:NixOS/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    mozilla = {
      url = "github:mozilla/nixpkgs-mozilla";
      flake = false;
    };

    emacs.url = "github:nix-community/emacs-overlay";
    utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , hardware
    , home-manager
    , mozilla
    , emacs
    , utils
    , flake-compat
    }:
      let
        mkSystem = { system, modules }:
          nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
              nixpkgs.nixosModules.notDetected
              (
                { config, ... }: {
                  # Pins nixpkgs of system to `inputs.nixpkgs`.
                  nix.registry.nixpkgs.flake = nixpkgs;
                  # Allows commands like `nix shell self#jmpunkt.emacs`
                  nix.registry.self.flake = self;
                  nixpkgs.overlays = overlays;
                }
              )
            ] ++ modules;
          };

        overlays = [
          (import mozilla)
          self.overlay
          emacs.overlay
        ];

        forAllSystems = utils.lib.eachDefaultSystem
          (system: { legacyPackages = import nixpkgs { inherit system overlays; }; });

        forx86Systems = utils.lib.eachSystem [ "x86_64-linux" "i686-linux" ] (
          system: {
            packages.iso = (
              nixpkgs.lib.nixosSystem {
                inherit system;
                modules = [ (import ./machines/iso/configuration.nix) ];
              }
            ).config.system.build.isoImage;

            apps.repl = utils.lib.mkApp {
              drv = nixpkgs.legacyPackages.${system}.writeShellScriptBin "repl" ''
                confnix=$(mktemp)
                echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
                trap "rm $confnix" EXIT
                nix repl $confnix
              '';
            };
          }
        );
      in
        forAllSystems // forx86Systems // {
          overlay = (final: prev: (import ./overlays/10-pkgs.nix final prev));

          nixosModules = {
            home-jonas = (
              { config, ... }: {
                home-manager.useGlobalPkgs = true;
                home-manager.useUserPackages = true;
                home-manager.users.jonas = ./home/jonas/home.nix;
              }
            );
          };

          nixosConfigurations = {
            alpha128 = mkSystem {
              system = "x86_64-linux";
              modules = [
                ./machines/alpha128/configuration.nix
                hardware.nixosModules.common-pc
                hardware.nixosModules.common-cpu-amd
                hardware.nixosModules.common-pc-ssd
                self.nixosModules.home-jonas
                home-manager.nixosModules.home-manager
              ];
            };

            gamma64 = mkSystem {
              system = "x86_64-linux";
              modules = [
                ./machines/gamma64/configuration.nix
                hardware.nixosModules.lenovo-thinkpad-e495
                hardware.nixosModules.common-pc-laptop-acpi_call
                hardware.nixosModules.common-pc-laptop-ssd
                self.nixosModules.home-jonas
                home-manager.nixosModules.home-manager
              ];
            };
          };
        };
}
