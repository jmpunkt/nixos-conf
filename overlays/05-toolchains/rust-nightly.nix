{ pkgs }:

(pkgs.rustChannelOf {
  date = "2020-05-18";
  channel = "nightly";
}).rust
