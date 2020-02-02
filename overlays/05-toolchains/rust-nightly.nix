{ mozillaTarball }:

let
  nixpkgs = import <nixpkgs> { overlays = [ (import mozillaTarball) ]; };
in
(nixpkgs.rustChannelOf { date = "2020-01-18"; channel = "nightly"; }).rust
