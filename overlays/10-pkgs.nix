self:
super:
# NOTICE: export everything with a prefix, making third-party
# dependencies immediately visible
{
  jmpunkt = ( super.jmpunkt or { } ) // ( super.callPackage ../pkgs { } );
  python3Packages =
    ( super.python3Packages or { } ) // { jmpunktPkgs = super.callPackage ../pkgs/python3Packages { }; };
  vimPlugins = ( super.vimPlugins or { } ) // { jmpunktPkgs = super.callPackage ../pkgs/vimPlugins { }; };
  vscode-extensions =
    ( super.vscode-extensions or { } ) // { jmpunktPkgs = super.callPackage ../pkgs/vscode-extensions { }; };
  emacsPackagesFor =
    emacs:
    (
      ( super.emacsPackagesFor emacs ).overrideScope'
        (
          eself:
          esuper:
          let
            manualPackages =
              esuper.manualPackages
                // {
                  jmpunktPkgs =
                    super.callPackage
                      ../pkgs/emacsPackages
                      {
                        inherit emacs;
                        emacsTrivialBuild = esuper.trivialBuild;
                      };
                };
          in
          esuper.override { inherit manualPackages; }
        )
    );
}
