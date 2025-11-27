{
  description = "My configuration as a flake";
  inputs = {
    stable.url = "github:NixOS/nixpkgs/nixos-25.11";
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    hardware.url = "github:NixOS/nixos-hardware";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
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
    emacs-igc-mirror = {
      url = "github:emacs-mirror/emacs/feature/igc";
      flake = false;
    };
    utils.url = "github:numtide/flake-utils";
    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "stable";
    };
  };
  outputs =
    {
      self,
      stable,
      unstable,
      hardware,
      home-manager,
      rust-overlay,
      emacs-overlay,
      utils,
      nix-index-database,
      ...
    }@inputs:
    let
      allPackagesOverlay = stable.lib.composeManyExtensions [
        (final: prev: {
          lib = prev.lib // {
            jmpunkt = lib;
          };
        })
        (import ./overlays/00-patch.nix)
        (import ./overlays/10-pkgs.nix)
        (import ./overlays/emacs-overlay-glue.nix { flake-inputs = inputs; })
      ];
      lib = import ./lib.nix {
        inherit
          self
          unstable
          stable
          home-manager
          ;
        minimumOverlays = [
          rust-overlay.overlays.default
          allPackagesOverlay
          emacs-overlay.overlays.package
        ];
      };
      inherit (lib)
        mkUnstableOverlay
        mkSystem
        mkPkgs
        packageSD
        packageISO
        packageSystem
        packageVM
        ;

      forAllSystems = utils.lib.eachDefaultSystem (system: {
        legacyPackages = mkPkgs system unstable [ mkUnstableOverlay ];
      });
      forx86Systems = utils.lib.eachSystem [ "x86_64-linux" "i686-linux" ] (system: {
        packages = {
          keyboard = self.legacyPackages.${system}.callPackage ./qmk { };
          iso-minimal = packageISO (mkSystem {
            inherit system inputs;
            nixpkgs = stable;
            modules = [ (import ./machines/iso-minimal/configuration.nix) ];
          });
          iso = packageISO (mkSystem {
            inherit system inputs;
            nixpkgs = stable;
            modules = [
              (import ./machines/iso/configuration.nix)
              self.nixosModules.home-jonas
            ];
          });
          sd-rpi2 = packageSD self.nixosConfigurations.rpi2;
          vm-alpha128 = packageVM (mkSystem {
            inherit system inputs;
            nixpkgs = stable;
            modules = [
              self.nixosModules.alpha128
              self.nixosModules.qemu-vm
            ];
          });
          vm-gamma64 = packageVM (mkSystem {
            inherit system inputs;
            nixpkgs = stable;
            modules = [
              self.nixosModules.gamma64
              self.nixosModules.qemu-vm
            ];
          });
        }
        // (builtins.mapAttrs (name: packageSystem) self.nixosConfigurations);
      });
    in
    (stable.lib.attrsets.recursiveUpdate forAllSystems forx86Systems)
    // {
      inherit lib;
      overlays.default = allPackagesOverlay;
      templates = import ./templates;
      nixosConfigurations = {
        alpha128 = mkSystem {
          inherit inputs;
          nixpkgs = stable;
          modules = [ self.nixosModules.alpha128 ];
        };
        gamma64 = mkSystem {
          inherit inputs;
          nixpkgs = stable;
          modules = [ self.nixosModules.gamma64 ];
        };
        rpi2 = mkSystem {
          inherit inputs;
          nixpkgs = unstable;
          modules = [
            ./machines/rpi2b/configuration.nix
            (
              { lib, ... }:
              {
                nixpkgs.system = lib.systems.examples.armv7l-hf-multiplatform;
                # target = "armv7l-hf-multiplatform";
                # system = "armv7l-linux";
              }
            )
          ];
        };
      };
      nixosModules = {
        home-unknown = (
          { config, ... }:
          {
            imports = [
              home-manager.nixosModules.home-manager
            ];
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.extraSpecialArgs = {
              inherit inputs;
              systemConfig = config;
            };
            home-manager.sharedModules = [
              nix-index-database.homeModules.nix-index
            ];
          }
        );
        home-jonas = (
          { ... }:
          {
            imports = [
              self.nixosModules.home-unknown
              ./configurations/users/jonas.nix
            ];
            home-manager.users.jonas = {
              imports = [
                ./home/jonas/home.nix
                ./home/jonas/emacs
              ];
            };
          }
        );
        home-jonas-yubikey = (
          { ... }:
          {
            home-manager.users.jonas = {
              imports = [
                ./home/jonas/yubikey
              ];
            };
          }
        );
        home-jonas-desktop-minimal = (
          { ... }:
          {
            home-manager.users.jonas = {
              imports = [
                ./home/jonas/desktop/minimal.nix
              ];
            };
          }
        );
        home-jonas-desktop-non-virtual = (
          { ... }:
          {
            home-manager.users.jonas = {
              imports = [
                ./home/jonas/desktop/non-virtual.nix
              ];
            };
          }
        );
        qemu-vm =
          { ... }:
          {
            imports = [
              "${stable}/nixos/modules/virtualisation/qemu-vm.nix"
              ./configurations/qemu-vm.nix

            ];
          };
        gamma64 =
          { ... }:
          {
            imports = [
              ./machines/gamma64/configuration.nix
              hardware.nixosModules.lenovo-thinkpad-e495
              hardware.nixosModules.common-pc-laptop-ssd
              self.nixosModules.home-jonas
              self.nixosModules.home-jonas-yubikey
              self.nixosModules.home-jonas-desktop-non-virtual
            ];
          };
        alpha128 =
          { ... }:
          {
            imports = [
              ./machines/alpha128/configuration.nix
              # ./configurations/vbox.nix
              hardware.nixosModules.common-pc
              hardware.nixosModules.common-pc-ssd
              hardware.nixosModules.common-cpu-amd
              hardware.nixosModules.common-cpu-amd-pstate
              hardware.nixosModules.common-gpu-amd
              self.nixosModules.home-jonas
              self.nixosModules.home-jonas-yubikey
              self.nixosModules.home-jonas-desktop-non-virtual
            ];
          };
      };
    };
}
