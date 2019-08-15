super: self:
{
  supermacs = super.pkgs.emacsWithPackages(epkgs:
  (with epkgs.melpaPackages; [
    evil
    use-package
    smartparens

    direnv

    hydra
    org-ref

    yasnippet
    yasnippet-snippets

    all-the-icons
    doom-modeline
    doom-themes

    pdf-tools
    latex-math-preview
    auctex-latexmk

    flycheck
    flycheck-irony
    flycheck-rust

    ivy
    counsel
    flyspell-correct-ivy

    company
    company-bibtex
    company-lua
    company-math
    company-emoji
    company-jedi
    company-quickhelp
    company-irony
    company-auctex
    company-reftex

    lsp-ui
    lsp-java
    lsp-haskell
    company-lsp

    toml-mode
    yaml-mode
    json-mode

    fish-mode
    nix-mode
    markdown-mode
    markdown-mode-plus
    pandoc-mode

    irony
    rust-mode
    elm-mode
    lua-mode
    haskell-mode
    web-mode
    typescript-mode
    js2-mode
  ])++ (with epkgs.elpaPackages; [
    undo-tree
    auctex
    org
  ])
  );
}
