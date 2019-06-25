self: super:

let
  crates = import ./Cargo.nix;
in
  {
    diesel_cli_postgres = (crates.diesel_cli {
      features = {
        default = false;
        postgres = true;
      };
    }).override {
      # NOTICE [2019-06-25]: NixOS currently only supports rust 1.32 and diesel
      # requires at least 1.34. Therefore, the Mozilla overlay with the latest
      # rust compiler is required.
      rust = self.latest.rustChannels.stable.rust;
      # crateOverrides = super.defaultCrateOverrides // {
      #   diesel = attrs: { buildInputs = [ super.pkgs.postgresql_11 ]; };
      # };
    };
  }
