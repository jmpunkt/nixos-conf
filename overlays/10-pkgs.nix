self: super:

{
  # export everything with a prefix, to mark missing dependencies inside the nix files
  jmpunkt = (super.jmpunkt or { }) // rec {
    nyan-project = super.callPackage ../pkgs/games/nyan-project { };
    vcmi = super.callPackage ../pkgs/games/vcmi { };
    veloren = super.callPackage ../pkgs/games/veloren { };
    openage = super.callPackage ../pkgs/games/openage { };
    opmon = super.callPackage ../pkgs/games/opmon { };
    tuxemon = super.callPackage ../pkgs/games/tuxemon { };
    gluon-lsp = super.callPackage ../pkgs/misc/gluon-lsp { };
    gluon = super.callPackage ../pkgs/misc/gluon-repl { };
    papyrus-rt = super.callPackage ../pkgs/applications/papyrus-rt { };
    gamemode = super.callPackage ../pkgs/misc/gamemode { };
    inih = super.callPackage ../pkgs/misc/inih { };
    kaffeine = super.callPackage ../pkgs/applications/kaffeine { };
  };

  # prefix additional libraries within the package
  python3Packages = (super.python3Packages or { }) // {
    jmpunkt = (super.python3Packages.jmpunkt or { }) // {
      netira = super.callPackage ../pkgs/python3/netira.nix { };
      pyscroll = super.callPackage ../pkgs/python3/pyscroll.nix { };
      pytmx = super.callPackage ../pkgs/python3/pytmx.nix { };
    };
  };
}
