self: super:

with super.lib;
let
  tarball = builtins.fetchGit {
    url = "https://github.com/mozilla/nixpkgs-mozilla";
    rev = "18cd4300e9bf61c7b8b372f07af827f6ddc835bb";
  };
in foldl' (flip extends) (_: super) [ (import tarball) ] self
