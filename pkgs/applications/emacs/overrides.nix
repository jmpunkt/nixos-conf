{emacsPackagesFor}: emacs: ((emacsPackagesFor emacs).overrideScope (eself: esuper: {
  # nothing here yet
  elpaPackages =
    esuper.elpaPackages
    // {
      org = esuper.elpaPackages.org.overrideAttrs (old: {
        patches = [];
      });
    };
  org = esuper.elpaPackages.org.overrideAttrs (old: {
    patches = [];
  });
}))
