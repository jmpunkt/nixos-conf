{ config, pkgs, ... }:

{
  imports = [ ./neovim/default.nix ]
  ++ (import ../../modules/all-home-manager.nix);

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

    jmpunkt.emacs
    jmpunkt.latex
  ];

  xdg.configFile = {
    "alacritty/alacritty.yml".text =
      builtins.readFile ./alacritty/alacritty.yml;
    # since emacs 27.1
    "emacs/init.el".text = builtins.readFile ./emacs/init.el;
  };

  home.file = {
    ".ssh/id_rsa.pub".text = builtins.readFile ./ssh/yubikey.pub;
    ".ssh/config".text = builtins.readFile ./ssh/config;
  };

  programs = {
    git = {
      enable = true;
      userName = "Jonas Meurer";
      userEmail = "jmpunkt@outlook.com";
      signing = {
        key = "4D78720A4358CC504F3EB45B26CDFB2E4DB6B136";
        signByDefault = true;
      };
      extraConfig = {
        core = {
          editor = "nvim";
          pager = "delta";
        };
        interactive.diffFilter = "delta --color-only";
        delta = {
          features = "line-numbers";
          whitespace-error-style = "22 reverse";
          decorations = {
            commit-decoration-style = "bold yellow box ul";
            file-style = "bold yellow ul";
            file-decoration-style = "none";
          };
        };
      };
    };
  };

  services.dropbox.enable = true;
}
