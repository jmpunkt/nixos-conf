{
  callPackage,
  emacs-overlay,
  libsForQt5,
}: {
  emacs = import ./applications/emacs { inherit emacs-overlay; };
  hunspellDicts = callPackage ./hunspellDicts {};
  oh-my-svg = libsForQt5.callPackage ./oh-my-svg {};
  html-to-svg = callPackage ./html-to-svg {};
}
