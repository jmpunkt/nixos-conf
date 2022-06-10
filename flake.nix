{
  description = "My configuration as a flake";
  inputs = {
    stable.url = "github:NixOS/nixpkgs/nixos-21.05";
    unstable.url = "github:NixOS/nixpkgs?rev=b70152ec071689a11c9d20acdb7cf60ce5202fad";
    hardware.url = "github:NixOS/nixos-hardware";
    home-manager = {
      url = "github:nix-community/home-manager/release-21.05";
      inputs.nixpkgs.follows = "stable";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "stable";
    };
    emacs-overlay.url = "github:nix-community/emacs-overlay";
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
    utils,
  }: let
    allPackagesOverlay = final: prev: (import ./overlays/10-pkgs.nix final prev);
    overlays = [rust-overlay.overlay allPackagesOverlay emacs-overlay.overlay];
    mkSystemCross = {
      host,
      target,
      system,
      modules,
      nixpkgs,
    }: let
      pkgs =
        (import
          nixpkgs
          {
            system = host;
          })
        .pkgs
        .pkgsCross
        ."${target}";
    in
      nixpkgs.lib.nixosSystem
      {
        inherit system;
        inherit pkgs;
        modules =
          [
            nixpkgs.nixosModules.notDetected
            (
              {config, ...}: {
                # Pins nixpkgs of system to `inputs.nixpkgs`.
                nix.registry.nixpkgs.flake = nixpkgs;
                # Allows commands like `nix shell self#jmpunkt.emacs`
                nix.registry.self.flake = self;
                nixpkgs.overlays = overlays ++ [(mkUnstableOverlay system)];
              }
            )
          ]
          ++ modules;
      };
    mkSystem = {
      system,
      modules,
      nixpkgs,
    }:
      nixpkgs.lib.nixosSystem
      {
        inherit system;
        modules =
          [
            nixpkgs.nixosModules.notDetected
            (
              {config, ...}: {
                # Pins nixpkgs of system to `inputs.nixpkgs`.
                nix.registry.nixpkgs.flake = nixpkgs;
                # Allows commands like `nix shell self#jmpunkt.emacs`
                nix.registry.self.flake = self;
                nixpkgs.overlays = overlays ++ [(mkUnstableOverlay system)];
              }
            )
          ]
          ++ modules;
      };
    mkPkgs = system: nixpkgs: additionalOverlays:
      import
      nixpkgs
      {
        inherit system;
        overlays = overlays ++ additionalOverlays;
        config.allowUnfree = true;
      };
    mkUnstableOverlay = system: (final: prev: {unstable = mkPkgs system unstable [];});
    forAllSystems =
      utils.lib.eachDefaultSystem
      (system: {legacyPackages = mkPkgs system stable [(mkUnstableOverlay system)];});
    forx86Systems = nixpkgs:
      utils.lib.eachSystem
      ["x86_64-linux" "i686-linux"]
      (
        system: {
          packages.iso =
            (
              mkSystem
              {
                inherit system nixpkgs;
                modules = [(import ./machines/iso/configuration.nix)];
              }
            )
            .config
            .system
            .build
            .isoImage;
          packages.sd-rpi2 =
            (mkSystemCross
              {
                host = "x86_64-linux";
                target = "armv7l-hf-multiplatform";
                system = "armv7l-linux";
                nixpkgs = unstable;
                modules = [
                  ./machines/rpi2b/configuration.nix
                ];
              })
            .config
            .system
            .build
            .sdImage;
          packages.iso-minimal =
            (
              mkSystem
              {
                inherit system nixpkgs;
                modules = [(import ./machines/iso-minimal/configuration.nix)];
              }
            )
            .config
            .system
            .build
            .isoImage;
          apps.repl =
            utils.lib.mkApp
            {
              drv =
                nixpkgs.legacyPackages.${system}.writeShellScriptBin
                "repl"
                ''
                  confnix=$(mktemp)
                  echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
                  trap "rm $confnix" EXIT
                  nix repl $confnix
                '';
            };
        }
      );
  in
    forAllSystems
    // (forx86Systems stable)
    // rec {
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
      nixosConfigurations = {
        alpha128 =
          mkSystem
          {
            system = "x86_64-linux";
            nixpkgs = unstable;
            modules = [
              ./machines/alpha128/configuration.nix
              hardware.nixosModules.common-pc
              hardware.nixosModules.common-cpu-amd
              hardware.nixosModules.common-pc-ssd
              self.nixosModules.home-jonas
              home-manager.nixosModules.home-manager
            ];
          };
        gamma64 =
          mkSystem
          {
            system = "x86_64-linux";
            nixpkgs = unstable;
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
