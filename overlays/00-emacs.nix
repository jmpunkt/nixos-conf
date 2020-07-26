self: super:

with super.lib;
let
  mozillaTarball = builtins.fetchGit {
    url = "https://github.com/nix-community/emacs-overlay";
    rev = "80e5a71f67da299dba1b3c4e00c9f16ba539766f";
  };
  overlay = self: super: { inherit mozillaTarball; };
in foldl' (flip extends) (_: super) [ (import mozillaTarball) overlay ] self
