{
  callPackage,
  libsForQt5,
  qt6Packages,
}: {
  nyan-project = callPackage ./games/nyan-project {};
  vcmi = callPackage ./games/vcmi {};
  openage = callPackage ./games/openage {};
  opmon = callPackage ./games/opmon {};
  tuxemon = callPackage ./games/tuxemon {};
  inih = callPackage ./misc/inih {};
  latex = callPackage ./misc/latex {};
  emacs = callPackage ./applications/emacs {};
  hunspellDicts = callPackage ./hunspellDicts {};
  verapdf = callPackage ./verapdf {};
  chatterino2-nigthly = qt6Packages.callPackage ./chatterino2 {};
  oh-my-svg = libsForQt5.callPackage ./oh-my-svg {};
}
