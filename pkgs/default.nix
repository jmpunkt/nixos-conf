{
  callPackage,
  emacs-overlay,
  libsForQt5,
}:
{
  # NOTE: Use `import` to ensure that we can still override the resulting
  # derivation.
  emacs = import ./applications/emacs { inherit emacs-overlay; };
  hunspellDicts = callPackage ./hunspellDicts { };
  oh-my-svg = libsForQt5.callPackage ./oh-my-svg { };
  html-to-svg = callPackage ./html-to-svg { };
  tree-sitter-typespec = callPackage ./tree-sitter-typespec { };
}
