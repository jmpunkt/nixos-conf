self: super:

{
  rustToolchain = super.callPackage ./rust.nix {};
  nightlyRustToolchain = super.callPackage ./rust-nightly.nix {};
  pythonToolchain = super.callPackage ./python.nix {};

  mozillaNightlyRustPlatform = super.makeRustPlatform {
    rustc = self.nightlyRustToolchain;
    cargo = self.nightlyRustToolchain;
  };

  mozillaRustPlatform = super.makeRustPlatform {
    rustc = self.rustToolchain;
    cargo = self.rustToolchain;
  };
}
