self: super:

{
  diesel =
  ((super.callPackage ./diesel.nix {}).diesel_cli {
    /*postgres = true;
    default = false;*/
    }).override {
    /*rust = super.pkgs.rust;
    crateOverrides = super.defaultCrateOverrides // {
    diesel = attrs: { buildInputs = with super.pkgs; [ postgresql_11 ]; };
    };*/
    };

  }
