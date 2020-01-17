self: super:

rec {
  nyan-project = super.callPackage ../pkgs/nyan-project/default.nix {};
  openage = super.callPackage ../pkgs/openage/default.nix {};
  vcmi = super.callPackage ../pkgs/vcmi/default.nix {};
  opmon = super.callPackage ../pkgs/opmon/default.nix {};
  python3Packages = {
    netira = super.callPackage ../pkgs/netira/default.nix {};
  } // super.python3Packages;
  tuxemon = super.callPackage ../pkgs/tuxemon/default.nix {};
  gluon-lsp = super.callPackage ../pkgs/gluon-lsp/default.nix {};
  gluon = super.callPackage ../pkgs/gluon-repl/default.nix {};
}
