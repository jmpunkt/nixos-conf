{
  lib,
  emacs,
  fetchFromGitHub,
  emacsPackagesFor,
}: {
  name,
  emacsPackageFn,
  initFile,
  emacsBuilder ? (emacsPackagesFor emacs),
}:
# Bundle a single Emacs file (e.g. init.el) as separate module with
# all its dependencies. The resulting derivation is an Emacs bundle
# with all dependencies plus the additionally generated module.
emacsBuilder.trivialBuild {
  pname = name;
  src = initFile;
  packageRequires = emacsPackageFn emacsBuilder;
}
