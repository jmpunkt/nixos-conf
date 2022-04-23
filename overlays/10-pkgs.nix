self: super:
# NOTICE: export everything with a prefix, making third-party
# dependencies immediately visible
{
  jmpunkt = (super.jmpunkt or {}) // (super.callPackage ../pkgs {});
  python3Packages =
    (super.python3Packages or {}) // {jmpunktPkgs = super.callPackage ../pkgs/python3Packages {};};
  vimPlugins = (super.vimPlugins or {}) // {jmpunktPkgs = super.callPackage ../pkgs/vimPlugins {};};
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

  # FIXME: remove when merged https://github.com/NixOS/nixpkgs/pull/164668
  cask = super.cask.overrideAttrs (oldAttrs: rec {
    installPhase = ''
      runHook preInstall
      mkdir -p $out/bin
      dir=$out/share/emacs/site-lisp/cask
      install -Dm444 -t $dir     *.el *.elc
      install -Dm555 -t $dir/bin bin/cask
      touch $out/.no-upgrade
      ln -s $dir/bin/cask $out/bin/cask
      runHook postInstall
    '';
  });
}
