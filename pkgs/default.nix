{
  callPackage,
  libsForQt5,
  qt6Packages,
}: {
  latex = callPackage ./misc/latex {};
  emacs = callPackage ./applications/emacs {};
  hunspellDicts = callPackage ./hunspellDicts {};
  verapdf = callPackage ./verapdf {};
  oh-my-svg = libsForQt5.callPackage ./oh-my-svg {};
  html-to-svg = callPackage ./html-to-svg {};
}
