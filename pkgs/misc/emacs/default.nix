{ stdenv
, buildEnv
, pkgs
, emacsGcc
, emacsPackagesFor
, symlinkJoin
, plantuml
, nodePackages
}:
let
  nixos-config-el = ''
    ;;; nixos-config.el --- short description                -*- lexical-binding: t; -*-

    ;;; Code:
    (setq-default org-plantuml-jar-path \"${plantuml}/lib/plantuml.jar\")
    (setq-default ob-mermaid-cli-path \"${nodePackages.jmpunkt.mermaid-cli}/bin/mmdc\")
    (setq-default mermaid-mmdc-location \"${nodePackages.jmpunkt.mermaid-cli}/bin/mmdc\")

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

  emacsConfiguration = stdenv.mkDerivation {
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

  emacsRuntime = buildEnv {
    name = "emacs-runtime";

    paths = with pkgs; [
      inkscape # org-mode:graphs
      imagemagick # org-mode:graphs
      graphviz-nox # org-mode:graphs
      nodePackages.jmpunkt.mermaid-cli # org-mode:graphs
      jre # required by plantuml
      ghostscript # LaTex EPS files
      unoconv # org-mode:odt
      zip # org-mode:odt
      languagetool # spelling
      nixpkgs-fmt # dev
      ccls # LSP
      rnix-lsp # LSP
      haskellPackages.haskell-lsp # LSP
      nodePackages.typescript-language-server # LSP
      texlab # LSP
      yaml-language-server # LSP
      nodePackages.typescript # dev
      discount
      maven # dev
      metals # LSP
      sbt # Scala
      cargo-outdated # dev

      jmpunkt.pythonToolchain # overlays/40-toolchains.nix
      jmpunkt.rustToolchain.stable # overlays/40-toolchains.nix
      jmpunkt.latex # custom latex suite
    ];
  };

  emacsLibexec = stdenv.mkDerivation {
    name = "emacs-libexec";
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out/bin

      ln -s ${emacsRuntime}/bin/* $out/bin
      find ${emacsGcc}/libexec/ -type f -exec ln -s {} $out/bin \;
    '';

    meta = {
      description = "Emulates a `libexec` directory for Emacs which includes the default `libexec` paths and additional dependencies.";
    };
  };
in
(emacsPackagesFor emacsGcc).emacsWithPackages (epkgs:
  (with epkgs.melpaPackages; [
    # Core
    evil
    use-package
    smartparens
    which-key
    ripgrep
    hl-todo
    direnv
    dashboard

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
    treemacs-evil
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

    # Search/Find
    ivy
    ivy-bibtex
    flx
    counsel
    counsel-projectile

    # Spelling
    langtool
    flyspell-correct
    flyspell-correct-ivy

    # Completion
    company
    company-bibtex
    company-math
    company-web
    company-quickhelp
    company-reftex

    # LSP
    lsp-ui
    lsp-java
    lsp-ivy
    lsp-treemacs
    lsp-metals

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
    sbt-mode
    scala-mode

    # Presentation
    markdown-mode
    pandoc-mode
    graphviz-dot-mode

    # Programming
    irony
    rustic
    haskell-mode
    web-mode
  ]) ++ (with epkgs.elpaPackages; [ undo-tree auctex seq ])
  ++ (with epkgs.orgPackages; [ org org-plus-contrib ])
  ++ (with pkgs; [ emacsConfiguration ]))
