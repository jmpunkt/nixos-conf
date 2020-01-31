self: super:

{
  jmpunkt = (super.jmpunkt or {}) // {
    emacs = super.pkgs.emacsWithPackages (epkgs:
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
      ]) ++ (with super.pkgs; [
        jmpunkt.latex
        inkscape # org-mode:graphs
        imagemagick # org-mode:graphs
        graphviz-nox # org-mode:graphs
        plantuml # org-mode:graphs
        jre # required by plantuml
        ghostscript # LaTex EPS files
        unoconv # org-mode:odt
        zip # org-mode:odt
        languagetool # spelling
        nixfmt # dev
        ccls # dev
        haskellPackages.haskell-lsp # dev
        nodePackages.typescript-language-server # dev
        nodePackages.typescript # dev
        pythonToolchain # overlays/40-toolchains.nix
        rustToolchain # overlays/40-toolchains.nix
        jmpunkt.rust-analyzer
        discount
      ])
    );
  };
}
