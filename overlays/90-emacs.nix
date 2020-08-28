self: super:

let
  nixos-config-el = with super; ''
    ;;; nixos-config.el --- short description                -*- lexical-binding: t; -*-

    ;;; Code:
    (setq-default org-plantuml-jar-path \"${plantuml}/lib/plantuml.jar\")
    (setq-default ob-mermaid-cli-path \"${nodePackages.mermaid-cli}/bin/mmdc\")
    (setq-default mermaid-mmdc-location \"${nodePackages.mermaid-cli}/bin/mmdc\")

    ;;; Add path such that `executable-find` is able to find them
    (add-to-list 'exec-path \"${emacsLibexec}/bin\")
    (setq-default exec-directory \"${emacsLibexec}/bin\")
    ;;; Add path such that `shell-command` can inherit `PATH`
    ;;; NOTICE: append it at the end of `PATH` to ensure that the user can override `PATH` (Linux eval from left to right in `PATH`?)
    (setenv \"PATH\"
            (let ((currentPath (getenv \"PATH\"))
                (additionalPath \"${emacsLibexec}/bin\"))
            (if currentPath
                (concat
                    (getenv \"PATH\")
                    \":\"
                    additionalPath)
                additionalPath)))

    (provide 'nixos-config)
    ;;; nixos-config.el ends here
  '';

  emacsConfiguration = super.stdenv.mkDerivation {
    name = "emacs-nixos-config";

    phases = [ "buildPhase" "installPhase" ];

    buildPhase = ''
      echo "${nixos-config-el}" > nixos-config.el
    '';

    installPhase = ''
      install -d "$out/share/emacs/site-lisp"
      install nixos-config.el "$out/share/emacs/site-lisp"
    '';

    meta = { description = "NixOS specific paths for Emacs."; };
  };

  emacsRuntime = super.buildEnv {
    name = "emacs-runtime";

    paths = with super.pkgs; [
      jmpunkt.latex # custom latex suite
      inkscape # org-mode:graphs
      imagemagick # org-mode:graphs
      graphviz-nox # org-mode:graphs
      nodePackages.mermaid-cli # org-mode:graphs
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
      discount
      maven
    ];
  };

  emacsLibexec = super.stdenv.mkDerivation {
    name = "emacs-libexec";
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out/bin

      ln -s ${emacsRuntime}/bin/* $out/bin
      find ${super.emacs}/libexec/ -type f -exec ln -s {} $out/bin \;
    '';
  };
in {
  jmpunkt = (super.jmpunkt or { }) // {
    emacs = super.emacsWithPackages (epkgs:
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
        ob-mermaid
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
        diff-hl

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
        ccls
        lsp-treemacs

        # Configuration
        bazel-mode
        yaml-mode
        json-mode
        fish-mode
        nix-mode
        graphql-mode
        meson-mode
        mermaid-mode
        dhall-mode

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
      ]) ++ (with epkgs.elpaPackages; [ undo-tree auctex seq ])
      ++ (with epkgs.orgPackages; [ org org-plus-contrib ])
      ++ (with super.pkgs; [ emacsConfiguration ]));
  };
}
