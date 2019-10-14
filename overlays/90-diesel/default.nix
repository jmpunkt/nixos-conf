self: super:

let
  crates = import ./Cargo.nix;
in
  {
    diesel_cli = (crates.diesel_cli { }).override {
      crateOverrides = super.defaultCrateOverrides // {
        diesel = attrs: {
          buildInputs = [
            super.pkgs.postgresql_11
            super.pkgs.sqlite
            super.pkgs.mariadb
          ];
        };
      };
    };
  }
