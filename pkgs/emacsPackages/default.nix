{ pkgs
, emacs
, emacsTrivialBuild
}:
{
  nixosPaths = pkgs.callPackage ./nixos-paths.nix { inherit emacs emacsTrivialBuild; };
  ligature = pkgs.callPackage ./ligature.nix { inherit emacsTrivialBuild; };
}
