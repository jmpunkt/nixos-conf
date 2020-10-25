{
  description = "My configuration as a flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.03";
    hardware.url = "github:NixOS/nixos-hardware";

    # TODO: Wait for 20.09 to integrate home-manager
    home-manager.url = "github:nix-community/home-manager";

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

  outputs = { self, nixpkgs, hardware, home-manager, mozilla, emacs, utils
    , flake-compat }:
    utils.lib.eachDefaultSystem (system: {
      legacyPackages = import nixpkgs {
        inherit system;
        overlays = [ (import mozilla) self.overlay emacs.overlay ];
      };

    }) // utils.lib.eachSystem [ "x86_64-linux" "i686-linux" ] (system: {
      packages.iso = (nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [ (import ./machines/iso/configuration.nix) ];
      }).config.system.build.isoImage;
    }) // {
      overlay = (final: prev: (import ./overlays/10-pkgs.nix final prev));

      nixosConfigurations = {
        alpha128 = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./machines/alpha128/configuration.nix
            nixpkgs.nixosModules.notDetected
            ({ config, ... }: { nix.registry.nixpkgs.flake = nixpkgs; })
          ];
        };

        alpha32 = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./machines/alpha32/configuration.nix
            hardware.nixosModules.lenovo-thinkpad-e495
            nixpkgs.nixosModules.notDetected
          ];
        };

        gamma64 = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./machines/gamma64/configuration.nix
            nixpkgs.nixosModules.notDetected
          ];
        };

      };
    };
}
