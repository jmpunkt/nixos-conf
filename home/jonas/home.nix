{
  config,
  pkgs,
  lib,
  xdgData,
  ...
}: {
  imports = import ../../modules/all-home-manager.nix;
  home.language = {
    base = "en_US.utf8";
    address = "de_DE.utf8";
    monetary = "de_DE.utf8";
    paper = "de_DE.utf8";
    time = "de_DE.utf8";
  };
  home.packages = with pkgs; [
    binutils-unwrapped
    dropbox-cli
    cryptsetup
    gnupg
    feh
    thunderbird
    audacious
    discord
    tdesktop
    tokei
    sqlite
    git
    hyperfine
    enchant
    hunspell
    hunspellDicts.en-us-large
    jmpunkt.hunspellDicts.de-de
    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    jmpunkt.chatterino2-nigthly
    jmpunkt.emacs
  ];
  xdg.configFile = {
    "emacs/init.el".text = builtins.readFile ./emacs/init.el;
    # since emacs 27.1
    "emacs/early-init.el".text = builtins.readFile ./emacs/early-init.el;
  };
  home.file = {
    ".ssh/id_rsa.pub".text = builtins.readFile ./ssh/yubikey.pub;
    ".ssh/config".text = builtins.readFile ./ssh/config;
  };
  manual.manpages.enable = false;
  home.sessionVariables = {
    MOZ_ENABLE_WAYLAND = 1;
  };
  programs = {
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    git = {
      enable = true;
      userName = "Jonas Meurer";
      userEmail = "jmpunkt@outlook.com";
      lfs.enable = true;
      signing = {
        key = "4D78720A4358CC504F3EB45B26CDFB2E4DB6B136";
        signByDefault = true;
      };
      extraConfig = {
        core = {
          editor = "emacs";
        };
      };
    };
  };
  services.dropbox.enable = true;
  home.stateVersion = "18.09";
}
