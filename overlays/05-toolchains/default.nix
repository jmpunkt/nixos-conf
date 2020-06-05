self: super:

rec {
  rustToolchains = super.callPackage ./rust.nix { };
  rustToolchain = rustToolchains.stable;
  nightlyRustToolchain = rustToolchains.nightly;
  pythonToolchain = super.callPackage ./python.nix { };

  mozillaNightlyRustPlatform = super.makeRustPlatform {
    rustc = self.rustToolchains.nightly;
    cargo = self.rustToolchains.nightly;
  };

  mozillaRustPlatform = super.makeRustPlatform {
    rustc = self.rustToolchains.stable;
    cargo = self.rustToolchains.stable;
  };
}
