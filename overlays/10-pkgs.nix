self: super:
# NOTICE: export everything with a prefix, making third-party
# dependencies immediately visible
{
  jmpunkt = (super.jmpunkt or {}) // (super.callPackage ../pkgs {});
  python3Packages =
    (super.python3Packages or {}) // {jmpunktPkgs = super.callPackage ../pkgs/python3Packages {};};
  nodePackages =
    (super.nodePackages or {})
    // {
      jmpunkt =
        super.callPackage
        ../pkgs/nodePackages {nodejs = super.nodePackages.nodejs;};
    };
  nodePackages_latest =
    (super.nodePackages_latest or {}) // {jmpunkt = super.callPackage ../pkgs/nodePackages {nodejs = super.nodePackages_latest.nodejs;};};
  vscode-extensions =
    (super.vscode-extensions or {}) // {jmpunktPkgs = super.callPackage ../pkgs/vscode-extensions {};};
  emacsPackagesFor = emacs: (
    (super.emacsPackagesFor emacs).overrideScope'
    (
      eself: esuper: let
        manualPackages =
          esuper.manualPackages
          // {
            jmpunktPkgs =
              super.callPackage
              ../pkgs/emacsPackages
              {
                inherit emacs;
                inherit (esuper) melpaBuild;
                passedPackages = esuper;
                emacsTrivialBuild = esuper.trivialBuild;
              };
          };
      in
        esuper.override {inherit manualPackages;}
    )
  );

  python39Packages =
    super.python39Packages
    // {
      pycurl = super.python39Packages.pycurl.overrideAttrs (old: {
        disabledTests =
          old.disabledTests
          ++ [
            "test_getinfo_raw_certinfo"
            "test_request_with_certinfo"
            "test_request_with_verifypeer"
            "test_request_without_certinfo"
          ];
      });
    };
}
