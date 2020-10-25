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

  python3Packages = (super.python3Packages or { }) // {
    jmpunkt = super.callPackage ../pkgs/python3Packages { };
  };

  vimPlugins = (super.vimPlugins or { }) // {
    jmpunkt = super.callPackage ../pkgs/vimPlugins { };
  };
}
