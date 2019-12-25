self: super:

rec {
  nyan-project = super.callPackage ../pkgs/nyan-project/default.nix {};
  openage = super.callPackage ../pkgs/openage/default.nix {};
  orsm-data = super.callPackage ../pkgs/osrm-data/default.nix {};
  gluon-lsp = super.callPackage ../pkgs/gluon-lsp/default.nix {};
  gluon = super.callPackage ../pkgs/gluon-repl/default.nix {};
}
