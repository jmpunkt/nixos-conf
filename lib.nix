{
  self,
  # stable version nixpkgs
  stable,
  # unstable version nixpkgs
  unstable,
  minimumOverlays,
}: let
  inherit (builtins) typeOf;
in rec {
  # provides a NixOS system which will be cross-compiled.
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
          overlays = minimumOverlays;
        })
      .pkgs
      .pkgsCross
      ."${target}";
  in
    nixpkgs.lib.nixosSystem
    {
      inherit system pkgs;
      modules =
        [
          nixpkgs.nixosModules.notDetected
          (
            {config, ...}: {
              # Pins nixpkgs of system to `inputs.nixpkgs`.
              nix.registry.nixpkgs.flake = nixpkgs;
              # Allows commands like `nix shell self#jmpunkt.emacs`
              nix.registry.self.flake = self;
              nixpkgs.overlays = minimumOverlays ++ [(mkUnstableOverlay system)];
            }
          )
        ]
        ++ modules;
    };
  # provides NixOS system with default settings.
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
              nixpkgs.overlays = minimumOverlays ++ [(mkUnstableOverlay system)];
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
      overlays = minimumOverlays ++ additionalOverlays;
      config.allowUnfree = true;
    };
  # Creates an overlay for a system which includes an attribute
  # `unstable` of all unstable nixpkgs.
  mkUnstableOverlay = system: (final: prev: {unstable = mkPkgs system unstable [];});
  # Provide the attribute path for building a SD image.
  packageSD = target:
    target
    .config
    .system
    .build
    .sdImage;
  # Provide the attribute path for building an ISO.
  packageISO = target:
    target
    .config
    .system
    .build
    .isoImage;
  # Provide the attribute path for building a NixOS system derivation.
  packageSystem = target:
    target.config.system.build.toplevel;
}
