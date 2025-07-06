{callPackage}: {
  nixosPaths = callPackage ./nixos-paths.nix {};
  eglot-x = callPackage ./eglot-x.nix {};
  typst-ts-mode = callPackage ./typst-ts-mode.nix {};
  copilot = callPackage ./copilot.nix {};
  p-search = callPackage ./p-search.nix {};
  reader = callPackage ./reader.nix {};
}
