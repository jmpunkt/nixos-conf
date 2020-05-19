self: super:

{
  # do not export libraries with prefix
  jmpunkt = (super.jmpunkt or { }) // {
    nyan-project = super.callPackage ../pkgs/nyan-project/games/default.nix { };
    vcmi = super.callPackage ../pkgs/vcmi/games/default.nix { };
    veloren = super.callPackage ../pkgs/veloren/games/default.nix { };
    openage = super.callPackage ../pkgs/openage/games/default.nix { };
    opmon = super.callPackage ../pkgs/opmon/games/default.nix { };
    python3Packages = (super.python3Packages or { }) // {
      netira = super.callPackage ../pkgs/netira/games/default.nix { };
    };
    tuxemon = super.callPackage ../pkgs/tuxemon/games/default.nix { };
    gluon-lsp = super.callPackage ../pkgs/gluon-lsp/default.nix { };
    gluon = super.callPackage ../pkgs/gluon-repl/default.nix { };
    rust-analyzer = super.callPackage ../pkgs/rust-analyzer/default.nix { };
    paprus-rt = super.callPackage ../pkgs/papyrus-rt/default.nix { };
  };
}
