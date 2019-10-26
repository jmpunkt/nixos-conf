super: self:

{
  supermacs = super.pkgs.emacsWithPackages (epkgs:
    (with epkgs.melpaPackages; [
      evil
      use-package
      smartparens
      which-key
      ripgrep
      hydra
      outshine

      org-fancy-priorities
      org-bullets
      org-ref
      org-noter
      ob-async
      toc-org
      ivy-bibtex
      pdf-tools
      biblio
      biblio-core
      parsebib
      direnv
      academic-phrases
      interleave
      langtool

      magit
      evil-magit

      projectile
      org-projectile

      treemacs
      treemacs-projectile

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
      counsel-projectile
      flyspell-correct-ivy

      company
      company-bibtex
      company-math
      company-emoji
      company-jedi
      company-web
      company-quickhelp
      company-irony
      company-auctex
      company-reftex

      lsp-ui
      lsp-java
      lsp-haskell
      company-lsp
      lsp-treemacs

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
      graphql-mode
      elm-mode
      haskell-mode
      web-mode
      js2-mode
      tide
      meson-mode
    ])++ (with epkgs.elpaPackages; [
      undo-tree
      auctex
      org
    ])
  );
}
