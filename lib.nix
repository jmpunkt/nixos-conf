{
  self,
  # stable version nixpkgs
  stable,
  # unstable version nixpkgs
  unstable,
  minimumOverlays,
}: let
  inherit (stable.lib.trivial) mod;
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
    inputs,
  }:
    nixpkgs.lib.nixosSystem
    {
      inherit system;
      specialArgs = {inherit inputs;};
      modules =
        [
          nixpkgs.nixosModules.notDetected
          (
            {config, ...}: {
              # Pins nixpkgs of system to `inputs.nixpkgs`.
              nix.registry.nixpkgs.flake = nixpkgs;
              # pin system nixpkgs to the same version as the flake
              # input (https://github.com/nix-community/nix-index/issues/167#issuecomment-989849343)
              nix.nixPath = ["nixpkgs=${nixpkgs}"];
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
  packageVM = target:
    target.config.system.build.vm;
  isLeapYear = year:
    ((mod year 4) == 0)
    && (((mod year 100) != 0) || ((mod year 400) == 0));
  yearsModUnixEpoch = timestamp: let
    helper = seconds: year: let
      daysRequired =
        if isLeapYear year
        then 366
        else 365;
      secondsRequired = daysRequired * 24 * 60 * 60;
    in
      if seconds <= secondsRequired
      then {
        year = year;
        day = (seconds / 60 / 60 / 24) + 1;
      }
      else helper (seconds - secondsRequired) (year + 1);
  in
    helper timestamp 1970;
  unixTimestampToDate = timestamp: let
    dayAndYear = yearsModUnixEpoch timestamp;
    year = dayAndYear.year;
    dayInYear = dayAndYear.day;
    daysInMonth = [
      31
      (
        if isLeapYear year
        then 29
        else 28
      )
      31
      30
      31
      30
      31
      31
      30
      31
      30
      31
    ];
    monthAndDay =
      builtins.foldl' (
        acc: daysInThisMonth:
          if acc.days <= daysInThisMonth
          then acc
          else {
            days = acc.days - daysInThisMonth;
            month = acc.month + 1;
          }
      ) {
        days = dayInYear;
        month = 1;
      }
      daysInMonth;
  in "${toString year}${toString monthAndDay.month}${toString monthAndDay.days}";
}
