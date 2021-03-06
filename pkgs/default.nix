{ callPackage }:

{
  nyan-project = callPackage ../pkgs/games/nyan-project { };
  vcmi = callPackage ../pkgs/games/vcmi { };
  veloren = callPackage ../pkgs/games/veloren { };
  openage = callPackage ../pkgs/games/openage { };
  opmon = callPackage ../pkgs/games/opmon { };
  tuxemon = callPackage ../pkgs/games/tuxemon { };
  gluon-lsp = callPackage ../pkgs/misc/gluon-lsp { };
  gluon = callPackage ../pkgs/misc/gluon-repl { };
  papyrus-rt = callPackage ../pkgs/applications/papyrus-rt { };
  gamemode = callPackage ../pkgs/applications/gamemode { };
  inih = callPackage ../pkgs/misc/inih { };
  kaffeine = callPackage ../pkgs/applications/kaffeine { };
  latex = callPackage ../pkgs/misc/latex { };
  emacs = callPackage ../pkgs/applications/emacs { };
  rustToolchain = callPackage ../pkgs/misc/rustToolchain { };
  pythonToolchain = callPackage ../pkgs/misc/pythonToolchain { };
  mozillaRustToolchain = callPackage ../pkgs/misc/mozillaRustToolchain { };
  hunspellDicts = callPackage ../pkgs/hunspellDicts { };
  verapdf = callPackage ../pkgs/applications/verapdf { };
}
