self: super:

with super.lib;
let
  mozillaTarball = builtins.fetchGit {
    url = "https://github.com/nix-community/emacs-overlay";
    rev = "aeeeefd7e3e5dee241d3307f73092c714517994e";
  };
  overlay = self: super: { inherit mozillaTarball; };
in foldl' (flip extends) (_: super) [ (import mozillaTarball) overlay ] self
