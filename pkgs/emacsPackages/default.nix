{ pkgs }:

{
  nixosPaths = pkgs.callPackage ./nixos-paths.nix { };
}
