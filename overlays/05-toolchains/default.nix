self: super:

{
  jmpunkt = (super.jmpunkt or { }) // rec {
    rustToolchain = super.callPackage ./rust.nix { };
    pythonToolchain = super.callPackage ./python.nix { };

    mozillaRustPlatform = {
      stable = super.makeRustPlatform {
        rustc = rustToolchain.stable.rustc;
        cargo = rustToolchain.stable.cargo;
      };

      nightly = super.makeRustPlatform {
        rustc = rustToolchain.nightly.rustc;
        cargo = rustToolchain.nightly.cargo;
      };
    };
  };
}
