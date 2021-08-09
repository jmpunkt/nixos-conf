{ lib, rust-bin }:

let
  withFeatures = nightly: default: default.override {
    extensions = [
      "cargo"
      "rustc"
      "clippy"
      "rust-std"
      "rust-src"
      "rust-docs"
      "rustfmt"
      "reproducible-artifacts"
    ] ++ (
      lib.optionals nightly [
        "rust-analyzer-preview"
        "miri"
      ]
    );
  };
in
{
  stable = withFeatures false rust-bin.stable.latest.default;
  nightly = withFeatures true rust-bin.nightly.latest.default;
}
