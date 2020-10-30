{ pkgs, runCommand, makeWrapper, latest, rustChannelOf }:
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
    runCommand "${channel.rust.name}-wrapped"
      {
        buildInputs = [ makeWrapper ];
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
in
{
  stable = (override (rustChannelOf {
    channel = "stable";
    date = "2020-10-08"; # 1.47
    sha256 = "adrApiScGGt4lZvGpEjbWiY5e10juOVRhyNpRSLTccI=";
  })).rust;
  nightly = (override (rustChannelOf {
    channel = "nightly";
    date = "2020-10-01";
    sha256 = "4Qdjc7PBLfexFQkU2SREnKxsHBwmErckKTUqbpengRU=";
  })).rust;
}
