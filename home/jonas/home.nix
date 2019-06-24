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
    python
    nodejs
    binutils-unwrapped
    nixify
  ];

  xdg.configFile."alacritty/alacritty.yml".text = builtins.readFile ./alacritty/alacritty.yml;
  home.file.".ssh/id_rsa.pub".source = ./yubico.pub;

  programs.git = {
    enable = true;
    userName = "Jonas Meurer";
    userEmail = "jmpunkt@outlook.de";
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
