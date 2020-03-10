self: super:

with super.lib;
let
  mozillaTarball = builtins.fetchGit {
    url = "https://github.com/mozilla/nixpkgs-mozilla";
    rev = "e912ed483e980dfb4666ae0ed17845c4220e5e7c";
  };
  overlay = self: super: { inherit mozillaTarball; };
in foldl' (flip extends) (_: super) [ (import mozillaTarball) overlay ] self
