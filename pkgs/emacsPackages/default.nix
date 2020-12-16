{ pkgs }:

{
  nixosPaths = pkgs.callPackage ./nixos-paths.nix { };
  ligature = pkgs.callPackage ./ligature.nix { };
}
