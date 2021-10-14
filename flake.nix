{
  description = "My configuration as a flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";

    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    hardware.url = "github:NixOS/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager/release-21.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs.url = "github:nix-community/emacs-overlay";

    utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , hardware
    , home-manager
    , rust-overlay
    , emacs
    , utils
    , unstable
    }:
      let
        allPackagesOverlay = final: prev:
          (import ./overlays/10-pkgs.nix final prev);

        overlays = [
          rust-overlay.overlay
          allPackagesOverlay
          emacs.overlay
        ];

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
                  nixpkgs.overlays = overlays ++ [ (mkUnstableOverlay system) ];
                }
              )
            ] ++ modules;
          };

        mkPkgs = system: src: additionalOverlays:
          import src {
            inherit system;
            overlays = overlays ++ additionalOverlays ++ [
              (
                let
                  noSysDirs = true;
                in
                  final: prev: {
                    my-cool-gcc = with prev; lowPrio (
                      wrapCC (
                        callPackage ./pkgs/gcc7.nix {
                          inherit noSysDirs;
                          nixpkgs = src;

                          profiledCompiler = false;

                          libcCross = if stdenv.targetPlatform != stdenv.buildPlatform then libcCross else null;
                          threadsCross = if stdenv.targetPlatform != stdenv.buildPlatform then threadsCross else null;

                          isl = if !stdenv.isDarwin then isl_0_17 else null;
                        }
                      )
                    );
                  }
              )
            ];
            config.allowUnfree = true;
          };

        mkUnstableOverlay = system:
          (final: prev: { unstable = mkPkgs system unstable []; });

        forAllSystems = utils.lib.eachDefaultSystem
          (
            system: {
              legacyPackages = mkPkgs system nixpkgs [ (mkUnstableOverlay system) ];
            }
          );

        forx86Systems = utils.lib.eachSystem
          [ "x86_64-linux" "i686-linux" ]
          (
            system: {
              packages.iso = (
                mkSystem {
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
          overlay = allPackagesOverlay;

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
