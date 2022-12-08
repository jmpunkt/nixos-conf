{
  callPackage,
  libsForQt5,
}: rec {
  nyan-project = callPackage ../pkgs/games/nyan-project {};
  vcmi = callPackage ../pkgs/games/vcmi {};
  openage = callPackage ../pkgs/games/openage {};
  opmon = callPackage ../pkgs/games/opmon {};
  tuxemon = callPackage ../pkgs/games/tuxemon {};
  gluon-lsp = callPackage ../pkgs/misc/gluon-lsp {};
  gluon = callPackage ../pkgs/misc/gluon-repl {};
  papyrus-rt = callPackage ../pkgs/applications/papyrus-rt {};
  gamemode = callPackage ../pkgs/applications/gamemode {};
  inih = callPackage ../pkgs/misc/inih {};
  kaffeine = libsForQt5.callPackage ../pkgs/applications/kaffeine {};
  latex = callPackage ../pkgs/misc/latex {};
  emacs = callPackage ../pkgs/applications/emacs {};
  rustToolchain = callPackage ../pkgs/misc/rustToolchain {};
  pythonToolchain = callPackage ../pkgs/misc/pythonToolchain {};
  hunspellDicts = callPackage ../pkgs/hunspellDicts {};
  verapdf = callPackage ../pkgs/applications/verapdf {};
  vpaint = libsForQt5.callPackage ../pkgs/misc/vpaint {};
  maui = libsForQt5.callPackage ../pkgs/applications/maui {};
  haruna = libsForQt5.callPackage ../pkgs/applications/haruna {};
  chatterino2-nigthly = libsForQt5.callPackage ../pkgs/applications/chatterino2 {};
  oh-my-svg = libsForQt5.callPackage ./applications/oh-my-svg {};
}
