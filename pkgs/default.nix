{
  callPackage,
  emacs-overlay,
  libsForQt5,
}:
{
  # NOTE: Use `import` to ensure that we can still override the resulting
  # derivation.
  emacs = import ./single/emacs { inherit emacs-overlay; };
  hunspellDicts = callPackage ./hunspellDicts { };
  oh-my-svg = libsForQt5.callPackage ./single/oh-my-svg { };
  html-to-svg = callPackage ./single/html-to-svg { };
  tree-sitter-typespec = callPackage ./single/tree-sitter-typespec { };
}
