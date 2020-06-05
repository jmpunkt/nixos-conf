{ pkgs }:

let
  override = { rust }:
    rust.override {
      targets = [ "x86_64-unknown-linux-gnu" ];
      extensions =
        [ "rust-src" "rust-analysis" "clippy-preview" "rustfmt-preview" ];
    };
in {
  stable = override { rust = pkgs.latest.rustChannels.stable.rust; };
  nightly = override {
    rust = (pkgs.rustChannelOf {
      date = "2020-06-04";
      channel = "nightly";
    }).rust;
  };
}
