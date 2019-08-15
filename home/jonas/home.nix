{ pkgs, config, ... }:
{
  imports = [
    ./neovim/default.nix
  ];

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
    nixify
    fzf

    supermacs
    ccls
    haskellPackages.haskell-lsp
    nodePackages.vscode-css-languageserver-bin
    nodePackages.vscode-html-languageserver-bin
    nodePackages.typescript-language-server
    nodePackages.typescript
    pythonToolchain # overlays/40-toolchains.nix
    rustToolchain # overlays/40-toolchains.nix

    # coc-nvim dependencies
    yarn
    nodejs
    ctags
  ];

  xdg.configFile."alacritty/alacritty.yml".text = builtins.readFile ./alacritty/alacritty.yml;
  home.file.".emacs.d" = {
    source = ./emacs;
    recursive = true;
  };

  home.file.".ssh/id_rsa.pub".text = builtins.readFile ./ssh/yubikey.pub;
  home.file.".ssh/config".text = builtins.readFile ./ssh/config;

  programs.git = {
    enable = true;
    userName = "Jonas Meurer";
    userEmail = "jmpunkt@outlook.com";
    signing = {
      key = "4D78720A4358CC504F3EB45B26CDFB2E4DB6B136";
      signByDefault = true;
    };
    extraConfig = {
      core.editor = "nvim";
    };
  };

  home.sessionVariables = {
    EDITOR = "nvim";
    BROWSER = "firefox";
    TERMINAL = "alacritty";
  };
}
