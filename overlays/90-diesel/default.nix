self: super:

let
  crates = import ./Cargo.nix;
  crates-io = import ./crates-io.nix;
  # features = (crates-io.features_.diesel_cli."1.4.0" crates.deps {
  #       features."diesel_cli"."1.4.0".deault = false;
  #       features."diesel_cli"."1.4.0".postgres = true;
  # });
in
  {
    diesel_cli = (crates.diesel_cli { }).override {
      # NOTICE [2019-06-25]: NixOS currently only supports rust 1.32 and diesel
      # requires at least 1.34. Therefore, the Mozilla overlay with the latest
      # rust compiler is required.
      rust = self.latest.rustChannels.stable.rust;
      crateOverrides = super.defaultCrateOverrides // {
        diesel = attrs: {
          # features = [
          #   "url" "sqlite" "postgres" "default"
          # ];
          buildInputs = [
            super.pkgs.postgresql_11
            super.pkgs.sqlite
            super.pkgs.mariadb
          ];
        };
      };
    };
  }
