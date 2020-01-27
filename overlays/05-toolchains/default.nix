self: super:

{
  rustToolchain = super.callPackage ./rust.nix {};
  pythonToolchain = super.callPackage ./python.nix {};

  mozillaRustPlatform = super.makeRustPlatform {
    rustc = self.rustToolchain;
    cargo = self.rustToolchain;
  };
}
