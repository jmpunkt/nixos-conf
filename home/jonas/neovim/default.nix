{ pkgs, stdenv, ... }:

{
  xdg.configFile."nvim/coc-settings.json".text = builtins.readFile ./coc-settings.json;

  programs.neovim = {
    enable = true;

    withPython3 = true;
    extraConfig = builtins.readFile ./neovim.vim;
    plugins = with pkgs.vimPlugins; [
      fzfWrapper
      fzf-vim
      vim-polyglot
      vimtex
      vim-cursorword
      vim-commentary
      denite-nvim
      vim-fugitive
      vim-airline
      vim-airline-themes
      vim-signature
      tagbar
      base16-vim
      vim-nix
      vim-sensible
      nerdtree
      nerdtree-git-plugin

      coc-nvim
      coc-rls
      coc-python
      coc-yaml
      coc-vimtex
      coc-json
      coc-css
      lexima
      quick-scope
      vim-searchindex
      vim-better-whitespace
    ];
  };
}
