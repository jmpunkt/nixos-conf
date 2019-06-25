{ pkgs, stdenv, ... }:

{
  xdg.configFile."nvim/coc-settings.json".text = builtins.readFile ./coc-settings.json;

  programs.neovim = {
    enable = true;

    configure = {
      customRC = builtins.readFile ./neovim.vim;
      vam.knownPlugins = pkgs.vimPlugins;
      vam.pluginDictionaries = [ { names = [
        "fzf-vim"
        "vim-polyglot"
        "vimtex"
        "vim-cursorword"
        "vim-commentary"
        "denite-nvim"
        "vim-fugitive"
        "vim-airline"
        "vim-airline-themes"
        "vim-signature"
        "tagbar"
        "base16-vim"
        "vim-nix"
        "vim-sensible"

        "defx-nvim"
        "defx-git"
        "coc-nvim"
        "lexima"
        "quick-scope"
        "vim-searchindex"
        "vim-better-whitespace"
      ];} ];
    };
  };
}
