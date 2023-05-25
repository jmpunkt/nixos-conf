{
  callPackage,
  emacs,
}: let
  emacsBinary = callPackage ./custom.nix {};
  emacsBundle = (callPackage ./overrides.nix {}) emacsBinary;
  packageFn = callPackage ./packages.nix {};
in
  emacsBundle.withPackages packageFn
# builder {
#   name = "jmpunkt-init";
#   emacsBuilder = emacsBundle;
#   emacsPackageFn = packageFn;
#   initFile = ./jmpunkt-init.el;
# }

