{
  pkgs,
  emacs,
  emacsTrivialBuild,
  passedPackages,
  melpaBuild,
}: {
  nixosPaths = pkgs.callPackage ./nixos-paths.nix {inherit emacs emacsTrivialBuild;};
  ligature = pkgs.callPackage ./ligature.nix {inherit emacsTrivialBuild;};
  eglot-x = pkgs.callPackage ./eglot-x.nix {
    inherit melpaBuild;
  };
  eglot-ltex = pkgs.callPackage ./eglot-ltex.nix {
    inherit melpaBuild;
  };
}
