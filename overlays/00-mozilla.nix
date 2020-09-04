self: super:

with super.lib;
let
  tarball = builtins.fetchGit {
    url = "https://github.com/mozilla/nixpkgs-mozilla";
    rev = "efda5b357451dbb0431f983cca679ae3cd9b9829";
  };
in foldl' (flip extends) (_: super) [ (import tarball) ] self
