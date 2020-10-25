self: super:

# NOTICE: export everything with a prefix, making third-party
# dependencies immediately visible
{
  jmpunkt = (super.jmpunkt or { }) // (super.callPackage ../pkgs { });

  python3Packages = (super.python3Packages or { }) // {
    jmpunkt = super.callPackage ../pkgs/python3Packages { };
  };

  nodePackages = (super.nodePackages or { }) // {
    jmpunkt = super.callPackage ../pkgs/nodePackages { };
  };

  vimPlugins = (super.vimPlugins or { }) // {
    jmpunkt = super.callPackage ../pkgs/vimPlugins { };
  };
}
