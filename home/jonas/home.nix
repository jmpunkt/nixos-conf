{ config, pkgs, ... }:

{
  imports = [ ./neovim/default.nix ./dropbox.nix ]
    ++ (import ../../modules/all-home-manager.nix);

  nixpkgs.config.allowUnfree = true;

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
    pdfpc
    steam
    skype
    minecraft

    jmpunkt.emacs
    jmpunkt.latex
  ];

  xdg.configFile."alacritty/alacritty.yml".text =
    builtins.readFile ./alacritty/alacritty.yml;

  home.file = {
    ".ssh/id_rsa.pub".text = builtins.readFile ./ssh/yubikey.pub;
    ".ssh/config".text = builtins.readFile ./ssh/config;
    ".emacs.d/init.el".text = builtins.readFile ./emacs/init.el;
  };

  programs.git = {
    enable = true;
    userName = "Jonas Meurer";
    userEmail = "jmpunkt@outlook.com";
    signing = {
      key = "4D78720A4358CC504F3EB45B26CDFB2E4DB6B136";
      signByDefault = true;
    };
    extraConfig = { core.editor = "nvim"; };
  };

  home.sessionVariables = {
    EDITOR = "nvim";
    BROWSER = "firefox";
    TERMINAL = "alacritty";
  };
}
