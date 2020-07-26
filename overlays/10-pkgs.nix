self: super:

{
  # export everything with a prefix, to mark missing dependencies inside the nix files
  jmpunkt = (super.jmpunkt or { }) // {
    nyan-project = super.callPackage ../pkgs/games/nyan-project/default.nix { };
    vcmi = super.callPackage ../pkgs/games/vcmi/default.nix { };
    veloren = super.callPackage ../pkgs/games/veloren/default.nix { };
    openage = super.callPackage ../pkgs/games/openage/default.nix { };
    opmon = super.callPackage ../pkgs/games/opmon/default.nix { };
    tuxemon = super.callPackage ../pkgs/games/tuxemon/default.nix { };
    gluon-lsp = super.callPackage ../pkgs/gluon-lsp/default.nix { };
    gluon = super.callPackage ../pkgs/gluon-repl/default.nix { };
    rust-analyzer = super.callPackage ../pkgs/rust-analyzer/default.nix { };
    papyrus-rt = super.callPackage ../pkgs/papyrus-rt/default.nix { };
  };

  # prefix additional libraries within the package
  python3Packages = (super.python3Packages or { }) // {
    jmpunkt = (super.python3Packages.jmpunkt or { }) // {
      netira = super.callPackage ../pkgs/python3/netira.nix { };
    };
  };
}
