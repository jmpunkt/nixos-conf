{ makeRustPlatform, rustToolchain }:

{
  stable = makeRustPlatform {
    rustc = rustToolchain.stable.rustc;
    cargo = rustToolchain.stable.cargo;
  };

  nightly = makeRustPlatform {
    rustc = rustToolchain.nightly.rustc;
    cargo = rustToolchain.nightly.cargo;
  };
}
