{ pkgs }:

(pkgs.rustChannelOf {
  date = "2020-01-18";
  channel = "nightly";
}).rust
