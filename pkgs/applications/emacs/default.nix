{callPackage}: let
  emacsBinary = callPackage ./custom.nix {};
  emacsBundleFn = callPackage ./overrides.nix {};
  emacsBundle = emacsBundleFn emacsBinary;
  packageFn = callPackage ./packages.nix {};
  builder = callPackage ./build-init.nix {};
in
  emacsBundle.withPackages packageFn
  # builder {
  #   name = "jmpunkt-init";
  #   emacsBuilder = emacsBundle;
  #   emacsPackageFn = packageFn;
  #   initFile = ./jmpunkt-init.el;
  # }
