{ pkgs }:

pkgs.latest.rustChannels.stable.rust.override {
  targets = [ "x86_64-unknown-linux-gnu" "wasm32-unknown-unknown" ];
  extensions = [
    "rust-src"
    "rls-preview"
    "rust-analysis"
    "clippy-preview"
    "rustfmt-preview"
  ];
}
