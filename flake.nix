{
  description = "My configuration as a flake";
  inputs = {
    stable.url = "github:NixOS/nixpkgs/nixos-24.05";
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    hardware.url = "github:NixOS/nixos-hardware";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "stable";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "stable";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "stable";
    };
    emacs-mirror = {
      url = "github:emacs-mirror/emacs";
      flake = false;
    };
    utils.url = "github:numtide/flake-utils";
  };
  outputs = {
    self,
    stable,
    unstable,
    hardware,
    home-manager,
    rust-overlay,
    emacs-overlay,
    emacs-mirror,
    utils,
  } @ inputs: let
    allPackagesOverlay = final: prev:
      (import ./overlays/10-pkgs.nix final prev)
      // ((import ./overlays/emacs-overlay-glue.nix {inherit emacs-overlay emacs-mirror;}) final prev);
    lib = import ./lib.nix {
      inherit self unstable stable;
      minimumOverlays = [
        rust-overlay.overlays.default
        allPackagesOverlay
        emacs-overlay.overlays.package
      ];
    };
    inherit
      (lib)
      mkUnstableOverlay
      mkSystem
      mkSystemCross
      mkPkgs
      packageSD
      packageISO
      packageSystem
      packageVM
      ;

    forAllSystems =
      utils.lib.eachDefaultSystem
      (system: {legacyPackages = mkPkgs system unstable [(mkUnstableOverlay system)];});
    forx86Systems =
      utils.lib.eachSystem
      ["x86_64-linux" "i686-linux"]
      (system: {
        apps = {
          repl =
            utils.lib.mkApp
            {
              drv =
                unstable.legacyPackages.${system}.writeShellScriptBin
                "repl"
                ''
                  confnix=$(mktemp)
                  echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
                  trap "rm $confnix" EXIT
                  nix repl $confnix
                '';
            };
        };
        packages = let
          rpi2System =
            mkSystemCross
            {
              host = system;
              target = "armv7l-hf-multiplatform";
              system = "armv7l-linux";
              nixpkgs = unstable;
              modules = [
                ./machines/rpi2b/configuration.nix
              ];
            };
        in {
          ee = emacs-mirror;
          keyboard = self.legacyPackages.${system}.callPackage ./qmk {};
          sd-rpi2 = packageSD rpi2System;
          rpi2 = packageSystem rpi2System;
          iso-minimal = packageISO (
            mkSystem
            {
              inherit system inputs;
              nixpkgs = stable;
              modules = [(import ./machines/iso-minimal/configuration.nix)];
            }
          );
          iso = packageISO (
            mkSystem
            {
              inherit system inputs;
              nixpkgs = stable;
              modules = [
                (import ./machines/iso/configuration.nix)
                self.nixosModules.home-unknown
                ({...}: {
                  home-manager.users.nixos = ./home/jonas/home.nix;
                })
              ];
            }
          );
          vm-alpha128 = packageVM (mkSystem
            {
              inherit system inputs;
              nixpkgs = stable;
              modules = [
                self.nixosModules.alpha128
                "${stable}/nixos/modules/virtualisation/qemu-vm.nix"
                ./configurations/qemu-vm.nix
              ];
            });
          vm-gamma64 = packageVM (mkSystem
            {
              inherit system inputs;
              nixpkgs = stable;
              modules = [
                self.nixosModules.gamma64
                "${stable}/nixos/modules/virtualisation/qemu-vm.nix"
                ./configurations/qemu-vm.nix
              ];
            });
        };
      });
    forx86_64Systems =
      utils.lib.eachSystem
      ["x86_64-linux"]
      (system: {
        packages = {
          alpha128 = packageSystem (
            mkSystem
            {
              inherit system inputs;
              nixpkgs = stable;
              modules = [
                self.nixosModules.alpha128
              ];
            }
          );
          gamma64 = packageSystem (mkSystem
            {
              inherit system inputs;
              nixpkgs = stable;
              modules = [self.nixosModules.gamma64];
            });
        };
      });
  in
    (
      stable.lib.attrsets.recursiveUpdate
      forAllSystems
      (
        stable.lib.attrsets.recursiveUpdate
        forx86Systems
        forx86_64Systems
      )
    )
    // {
      overlays.default = allPackagesOverlay;
      templates = import ./templates;
      nixosModules = {
        home-unknown = (
          {config, ...}: {
            imports = [
              home-manager.nixosModules.home-manager
            ];
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = {
              inherit inputs;
              systemConfig = config;
            };
          }
        );
        home-jonas = (
          {config, ...}: {
            imports = [self.nixosModules.home-unknown];
            home-manager.users.jonas = ./home/jonas/home.nix;
          }
        );
        gamma64 = {config, ...}: {
          imports = [
            ./machines/gamma64/configuration.nix
            hardware.nixosModules.lenovo-thinkpad-e495
            hardware.nixosModules.common-pc-laptop-acpi_call
            hardware.nixosModules.common-pc-laptop-ssd
            self.nixosModules.home-jonas
          ];
        };
        alpha128 = {conifg, ...}: {
          imports = [
            ./machines/alpha128/configuration.nix
            hardware.nixosModules.common-pc
            hardware.nixosModules.common-pc-ssd
            hardware.nixosModules.common-cpu-amd
            hardware.nixosModules.common-cpu-amd-pstate
            hardware.nixosModules.common-gpu-amd
            self.nixosModules.home-jonas
          ];
        };
      };
    };
}
