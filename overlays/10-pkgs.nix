self: super:
# NOTICE: export everything with a prefix, making third-party
# dependencies immediately visible
{
  jmpunkt = (super.jmpunkt or { }) // (super.callPackage ../pkgs { });
  python3Packages = (super.python3Packages or { }) // {
    jmpunktPkgs = super.callPackage ../pkgs/python3Packages { };
  };
  nodePackages = (super.nodePackages or { }) // {
    jmpunkt = super.callPackage ../pkgs/nodePackages { nodejs = super.nodePackages.nodejs; };
  };
  nodePackages_latest = (super.nodePackages_latest or { }) // {
    jmpunkt = super.callPackage ../pkgs/nodePackages { nodejs = super.nodePackages_latest.nodejs; };
  };
  vscode-extensions = (super.vscode-extensions or { }) // {
    jmpunktPkgs = super.callPackage ../pkgs/vscode-extensions { };
  };
  linuxPackages = (super.linuxPackages or { }) // {
    jmpunkt = super.linuxPackages.callPackage ../pkgs/linuxPkgs { };
  };
  linuxPackages_latest = (super.linuxPackages_latest or { }) // {
    jmpunkt = super.linuxPackages_latest.callPackage ../pkgs/linuxPkgs { };
  };
  emacsPackagesFor =
    emacs:
    ((super.emacsPackagesFor emacs).overrideScope (
      eself: esuper:
      let
        manualPackages = esuper.manualPackages // {
          jmpunktPkgs = eself.callPackage ../pkgs/emacsPackages {
          };
        };
      in
      esuper.override { inherit manualPackages; }
    ));
}
