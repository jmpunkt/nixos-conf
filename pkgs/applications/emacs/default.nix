{callPackage}: let
  emacsBinary = callPackage ./custom.nix {};
  emacsBundle = (callPackage ./overrides.nix {}) emacsBinary;
in
  emacsBinary
