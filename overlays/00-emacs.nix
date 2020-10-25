self: super:

with super.lib;
let
  tarball = builtins.fetchGit {
    url = "https://github.com/nix-community/emacs-overlay";
    rev = "92d0791b08f37221799bfb6a263755b35a72fddb";
  };
in foldl' (flip extends) (_: super) [ (import tarball) ] self
