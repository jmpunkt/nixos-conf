{ lib
, makeRustPlatform
, rust-bin
}:
let
  withFeatures =
    nightly:
    default:
    default.override
      {
        extensions =
          [ "cargo" "rustc" "clippy" "rust-std" "rust-src" "rust-docs" "rustfmt" "reproducible-artifacts" ]
            ++ ( lib.optionals nightly [ "rust-analyzer-preview" "miri" ] );
      };
in
rec {
  stable = withFeatures false rust-bin.stable.latest.default;
  stablePlatform =
    makeRustPlatform
      {
        rustc = stable;
        cargo = stable;
      };
  nightly = withFeatures true rust-bin.nightly.latest.default;
  nightlyPlatform =
    makeRustPlatform
      {
        rustc = nightly;
        cargo = nightly;
      };
}
