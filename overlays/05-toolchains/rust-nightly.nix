{ pkgs }:

(pkgs.rustChannelOf {
  date = "2020-05-01";
  channel = "nightly";
}).rust
