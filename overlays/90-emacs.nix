super: self:

{
  supermacs = super.pkgs.emacsWithPackages (epkgs:
    (with epkgs.melpaPackages; [
      # Core
      evil
      use-package
      smartparens
      which-key
      ripgrep
      hydra
      outshine
      hl-todo
      direnv

      # Org
      org-fancy-priorities
      org-bullets
      org-ref
      org-noter
      ob-async
      toc-org
      pdf-tools

      # Bibliography
      biblio
      biblio-core
      parsebib

      # LaTeX
      latex-math-preview
      auctex-latexmk

      # PDF
      interleave
      pdf-tools

      # Git
      magit
      magit-todos
      evil-magit

      # Tree
      treemacs
      treemacs-projectile
      projectile
      org-projectile

      # Templates
      yasnippet
      yasnippet-snippets

      # UI
      all-the-icons
      doom-modeline
      doom-themes

      # Linter
      flycheck
      flycheck-irony
      flycheck-rust

      # Search/Find
      ivy
      ivy-bibtex
      flx
      counsel
      counsel-projectile

      # Spelling
      langtool
      flyspell-correct-ivy

      # Completion
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

      # LSP
      lsp-ui
      lsp-java
      lsp-ivy
      lsp-haskell
      company-lsp
      lsp-treemacs

      # Configuration
      bazel-mode
      toml-mode
      yaml-mode
      json-mode
      fish-mode
      nix-mode
      graphql-mode
      meson-mode

      # Presentation
      markdown-mode
      markdown-mode-plus
      pandoc-mode
      graphviz-dot-mode

      # Programming
      irony
      rust-mode
      elm-mode
      haskell-mode
      web-mode
      js2-mode
      tide
    ])++ (with epkgs.elpaPackages; [
      undo-tree
      auctex
    ]) ++ (with epkgs.orgPackages; [
      org
      org-plus-contrib
    ])
  );
}
