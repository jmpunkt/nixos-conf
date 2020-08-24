self: super:

with super.lib;
let
  mozillaTarball = builtins.fetchGit {
    url = "https://github.com/nix-community/emacs-overlay";
    rev = "2d1b44e897716fc7c75b0f674193740b45cafd62";
  };
  overlay = self: super: { inherit mozillaTarball; };
in foldl' (flip extends) (_: super) [ (import mozillaTarball) overlay ] self
