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
  eglot-booster = pkgs.callPackage ./eglot-booster.nix {
    inherit melpaBuild;
  };
  eglot-ltex = pkgs.callPackage ./eglot-ltex.nix {
    inherit melpaBuild;
  };
  typst-ts-mode = pkgs.callPackage ./typst-ts-mode.nix {
    inherit melpaBuild;
  };
  dart-ts-mode = pkgs.callPackage ./dart-ts-mode.nix {
    inherit melpaBuild;
  };
  ox-typst = pkgs.callPackage ./ox-typst.nix {
    inherit melpaBuild;
  };
}
