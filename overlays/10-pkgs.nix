self: super:

{
  # do not export libraries with prefix
  nyan-project = super.callPackage ../pkgs/nyan-project/default.nix { };
  vcmi = super.callPackage ../pkgs/vcmi/default.nix { };
  veloren = super.callPackage ../pkgs/veloren/default.nix { };

  jmpunkt = (super.jmpunkt or { }) // {
    openage = super.callPackage ../pkgs/openage/default.nix { };
    opmon = super.callPackage ../pkgs/opmon/default.nix { };
    python3Packages = (super.python3Packages or { }) // {
      netira = super.callPackage ../pkgs/netira/default.nix { };
    };
    tuxemon = super.callPackage ../pkgs/tuxemon/default.nix { };
    gluon-lsp = super.callPackage ../pkgs/gluon-lsp/default.nix { };
    gluon = super.callPackage ../pkgs/gluon-repl/default.nix { };
    rust-analyzer = super.callPackage ../pkgs/rust-analyzer/default.nix { };
    osrm = super.callPackage ../pkgs/osrm/default.nix { };
  };
}
