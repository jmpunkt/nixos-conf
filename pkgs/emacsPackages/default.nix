{
  pkgs,
  emacs,
  emacsTrivialBuild,
  passedPackages,
  melpaBuild,
}: {
  nixosPaths = pkgs.callPackage ./nixos-paths.nix {inherit emacs emacsTrivialBuild;};
  eglot-x = pkgs.callPackage ./eglot-x.nix {
    inherit melpaBuild;
  };
  typst-ts-mode = pkgs.callPackage ./typst-ts-mode.nix {
    inherit melpaBuild;
  };
  copilot = pkgs.callPackage ./copilot.nix {
    inherit melpaBuild;
  };
  xdg-appmenu = pkgs.callPackage ./xdg-appmenu.nix {
    inherit melpaBuild;
  };
}
