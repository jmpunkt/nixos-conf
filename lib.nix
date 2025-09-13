{
  self,
  # stable version nixpkgs
  stable,
  # unstable version nixpkgs
  home-manager,
  unstable,
  minimumOverlays,
}:
let
  inherit (stable.lib.trivial) mod;
in
rec {
  defaultConfig =
    nixpkgs:
    (
      {
        lib,
        ...
      }:
      {
        # enable flakes
        nix.settings.experimental-features = [
          "nix-command"
          "flakes"
        ];
        # disable default flake registry entries
        nix.settings.flake-registry = "";
        # disable channels, since we are using flakes
        nix.channel.enable = lib.mkForce false;
        # Pins nixpkgs of system to `inputs.nixpkgs`.
        nixpkgs.flake.setNixPath = true;
        # Allows commands like `nix shell self#jmpunkt.emacs`
        nix.registry.self.flake = self;
        nixpkgs.overlays = minimumOverlays ++ [ mkUnstableOverlay ];
        nix.settings.nix-path = lib.mkForce [
          "nixpkgs=${nixpkgs}"
          "home-manager=${home-manager}"
        ];
        nix.nixPath = lib.mkForce [
          "nixpkgs=${nixpkgs}"
          "home-manager=${home-manager}"
        ];
      }
    );
  # provides NixOS system with default settings.
  mkSystem = stable.lib.makeOverridable (
    {
      modules,
      nixpkgs,
      inputs,
    }:
    nixpkgs.lib.nixosSystem {
      specialArgs = { inherit inputs; };
      modules = [
        nixpkgs.nixosModules.notDetected
        (defaultConfig nixpkgs)
      ]
      ++ modules;
    }
  );
  mkPkgs =
    system: nixpkgs: additionalOverlays:
    import nixpkgs {
      inherit system;
      overlays = minimumOverlays ++ additionalOverlays;
    };
  # Creates an overlay for a system which includes an attribute
  # `unstable` of all unstable nixpkgs.
  mkUnstableOverlay = final: prev: {
    unstable = mkPkgs (final.pkgs.hostPlatform.system) unstable [ ];
  };
  # Provide the attribute path for building a SD image.
  packageSD = target: target.config.system.build.sdImage;
  # Provide the attribute path for building an ISO.
  packageISO = target: target.config.system.build.isoImage;
  # Provide the attribute path for building a NixOS system derivation.
  packageSystem = target: target.config.system.build.toplevel;
  packageVM = target: target.config.system.build.vm;
  isLeapYear = year: ((mod year 4) == 0) && (((mod year 100) != 0) || ((mod year 400) == 0));
  yearsModUnixEpoch =
    timestamp:
    let
      helper =
        seconds: year:
        let
          daysRequired = if isLeapYear year then 366 else 365;
          secondsRequired = daysRequired * 24 * 60 * 60;
          dayFloat = seconds / 60.0 / 60.0 / 24.0;
        in
        if seconds <= secondsRequired then
          {
            year = year;
            day = (builtins.floor dayFloat) + 1;
            reminder = truncateFloat dayFloat;
          }
        else
          helper (seconds - secondsRequired) (year + 1);
    in
    helper timestamp 1970;
  truncateFloat = float: float - (builtins.floor float);
  # Pads a number with a leading zero if it is less than 10.
  pad2Number = number: if number < 10 then "0${toString number}" else toString number;
  # Converts a Unix timestamp to a date-time string (YYYYMMDD.HHMM).
  unixTimestampToDateTime =
    timestamp:
    let
      dayAndYear = yearsModUnixEpoch timestamp;
      year = dayAndYear.year;
      dayInYear = dayAndYear.day;
      daysInMonth = [
        31
        (if isLeapYear year then 29 else 28)
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
        builtins.foldl'
          (
            acc: daysInThisMonth:
            if acc.days <= daysInThisMonth then
              acc
            else
              {
                days = acc.days - daysInThisMonth;
                month = acc.month + 1;
              }
          )
          {
            days = dayInYear;
            month = 1;
          }
          daysInMonth;
      hour = dayAndYear.reminder * 24.0;
      minute = (truncateFloat hour) * 60.0;
    in
    "${toString year}${pad2Number monthAndDay.month}${pad2Number monthAndDay.days}.${pad2Number (builtins.floor hour)}${pad2Number (builtins.floor minute)}";
}
