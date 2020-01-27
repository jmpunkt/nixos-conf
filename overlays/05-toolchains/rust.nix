{ mozOverlay ? (builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz), ...}:

let
  nixpkgs = import <nixpkgs> { overlays = [ (import mozOverlay) ]; };
in
nixpkgs.latest.rustChannels.stable.rust.override {
  targets = [
    "x86_64-unknown-linux-gnu"
    "wasm32-unknown-unknown"
  ];
  extensions = [
    "rust-src"
    "rls-preview"
    "rust-analysis"
    "clippy-preview"
    "rustfmt-preview"
  ];
}
