{ pkgs }:

let
  override = channel:
    channel // {
      rust = channel.rust.override {
        targets = [ "x86_64-unknown-linux-gnu" ];
        extensions = [ "rust-src" "clippy-preview" "rustfmt-preview" ];
      };
    };

  # wraps a rust toolchain and adds custom srcs to the environment path.
  wrapped = channel:
    pkgs.runCommand "${channel.rust.name}-wrapped" {
      buildInputs = [ pkgs.makeWrapper ];
    } ''
      mkdir $out
      ln -s ${channel.rust}/* $out
      rm $out/bin
      mkdir $out/bin
      ln -s ${channel.rust}/bin/* $out/bin
      rm $out/bin/rust-analyzer
      makeWrapper ${channel.rust}/bin/rust-analyzer $out/bin/rust-analyzer \
          --set RUST_SRC_PATH ${channel.rust-src}/lib/rustlib/src/rust/src
    '';
in {
  stable = (override pkgs.latest.rustChannels.stable).rust;
  nightly = (override (pkgs.rustChannelOf {
    date = "2020-09-04";
    channel = "nightly";
  })).rust;
}
