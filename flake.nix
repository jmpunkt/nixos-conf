{
  description = "My configuration as a flake";
  inputs = {
    stable.url = "github:NixOS/nixpkgs/nixos-22.05";
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    hardware.url = "github:NixOS/nixos-hardware";
    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "stable";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "unstable";
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "unstable";
    };
    utils.url = "github:numtide/flake-utils";
    tsi = {
      url = "git+file:///home/jonas/workspace/tree-sitter-indexer";
      inputs.nixpkgs.follows = "unstable";
      inputs.rust-overlay.follows = "rust-overlay";
    };
  };
  outputs = {
    self,
    stable,
    unstable,
    hardware,
    home-manager,
    rust-overlay,
    emacs-overlay,
    tsi,
    utils,
  }: let
    allPackagesOverlay = final: prev:
      (import ./overlays/10-pkgs.nix final prev)
      // {
        inherit tsi;
      };
    lib = import ./lib.nix {
      inherit self unstable stable;
      minimumOverlays = [
        rust-overlay.overlays.default
        allPackagesOverlay
        emacs-overlay.overlay
        tsi.overlays.default
      ];
    };
    inherit (lib) mkUnstableOverlay mkSystem mkSystemCross mkPkgs packageSD packageISO packageSystem;
    forAllSystems =
      utils.lib.eachDefaultSystem
      (system: {legacyPackages = mkPkgs system stable [(mkUnstableOverlay system)];});
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

          # switch the current live system (not persistent)
          switch-live =
            utils.lib.mkApp
            {
              drv =
                unstable.legacyPackages.${system}.writeShellScriptBin
                "switch-local"
                  ''

tmpDir=/tmp/$(date +%s)-nix-flake-builder
mkdir "$tmp
name=$(hostname)
nix build ${self}#$name

rm -r "$tmpdir"
                  # doas result/bin/switch-to-configuration switch
                '';
            };
          # switch the system for next boot (persistent)
          switch-boot =
            utils.lib.mkApp
            {
              drv =
                unstable.legacyPackages.${system}.writeShellScriptBin
                "switch-boot"
                ''
                  doas nix-env -p /nix/var/nix/profiles/system --set ./result
                  doas result/bin/switch-to-configuration boot
                '';
            };
          # switch the live system and keep changes for next boot (persistent)
          switch-live-boot =
            utils.lib.mkApp
            {
              drv =
                unstable.legacyPackages.${system}.writeShellScriptBin
                "switch-local"
                ''
                  doas result/bin/switch-to-configuration boot
                '';
            };
        };
        packages = {
          sd-rpi2 = packageSD (mkSystemCross
            {
              host = system;
              target = "armv7l-hf-multiplatform";
              system = "armv7l-linux";
              nixpkgs = unstable;
              modules = [
                ./machines/rpi2b/configuration.nix
              ];
            });
          iso-minimal = packageISO (
            mkSystem
            {
              inherit system;
              nixpkgs = stable;
              modules = [(import ./machines/iso-minimal/configuration.nix)];
            }
          );
          iso = packageISO (
            mkSystem
            {
              inherit system;
              nixpkgs = stable;
              modules = [(import ./machines/iso/configuration.nix)];
            }
          );
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
              inherit system;
              nixpkgs = unstable;
              modules = [
                ./machines/alpha128/configuration.nix
                hardware.nixosModules.common-pc
                hardware.nixosModules.common-cpu-amd
                hardware.nixosModules.common-pc-ssd
                self.nixosModules.home-jonas
                home-manager.nixosModules.home-manager
              ];
            }
          );
          gamma64 = packageSystem (mkSystem
            {
              inherit system;
              nixpkgs = unstable;
              modules = [
                ./machines/gamma64/configuration.nix
                hardware.nixosModules.lenovo-thinkpad-e495
                hardware.nixosModules.common-pc-laptop-acpi_call
                hardware.nixosModules.common-pc-laptop-ssd
                self.nixosModules.home-jonas
                home-manager.nixosModules.home-manager
              ];
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
        home-jonas = (
          {config, ...}: {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.jonas = ./home/jonas/home.nix;
          }
        );
      };
    };
}
