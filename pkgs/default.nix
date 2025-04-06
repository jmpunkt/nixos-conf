{
  callPackage,
  libsForQt5,
  qt6Packages,
}: {
  emacs = callPackage ./applications/emacs {};
  hunspellDicts = callPackage ./hunspellDicts {};
  oh-my-svg = libsForQt5.callPackage ./oh-my-svg {};
  html-to-svg = callPackage ./html-to-svg {};
}
